extern crate r2d2;
extern crate rusqlite;
#[macro_use]
extern crate nickel;
extern crate hyper;
extern crate nickel_sqlite;
extern crate rustc_serialize;

use r2d2::NopErrorHandler;

use rusqlite::{Connection, Row};
use rusqlite::Error::SqliteFailure;

use nickel::{Nickel, Request, JsonBody, HttpRouter, MediaType};
use hyper::header::{ContentType, Host, Location};
use nickel::status::StatusCode;

use nickel_sqlite::{SqliteMiddleware, SqliteRequestExtensions};

use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Player {
    name: String,
    level: i64,
}

impl<'a> From<&'a Row<'a>> for Player {
    fn from(row: &Row) -> Player {
        Player {
            name: row.get(0),
            level: row.get(1),
        }
    }
}

fn get_players(db: &Connection) -> Vec<Player> {
    db.prepare("SELECT name, level FROM player")
      .unwrap()
      .query_map(&[], |row| Player::from(row))
      .unwrap()
      .map(Result::unwrap)
      .collect()
}

fn get_player<S: Into<String>>(db: &Connection, player: S) -> Option<Player> {
    db.prepare("SELECT name, level FROM player WHERE name = ?")
      .unwrap()
      .query_map(&[&player.into()], |row| Player::from(row))
      .unwrap()
      .map(Result::unwrap)
      .next()
}

fn delete_player<S: Into<String>>(db: &Connection, player: S) -> Result<i32, rusqlite::Error> {
    db.execute("DELETE FROM player WHERE name = ?", &[&player.into()])
}

fn create_player<S: Into<String>>(db: &Connection,
                                  name: S,
                                  level: i64)
                                  -> Result<i32, rusqlite::Error> {
    db.execute("INSERT INTO player (name, level) VALUES (?, ?)",
               &[&name.into(), &level])
}

fn migrate(db: &Connection) {
    match db.execute("CREATE TABLE player (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, \
                      level INTEGER)",
                     &[]) {
        Ok(_) => {
            for (name, level) in vec![("Lance", 1), ("Sally", 2), ("Aki", 3), ("Maria", 4)] {
                create_player(db, name, level).unwrap();
            }
        }
        Err(SqliteFailure(_, Some(ref reason))) if reason == "table player already exists" => {}
        Err(err) => panic!("{:?}", err),
    }
}

fn url_for_request(req: &Request) -> String {
    let Host { ref hostname, port } = *req.origin.headers.get::<Host>().unwrap();
    let port = port.map_or("".to_string(), |port| format!(":{}", port));
    format!("http://{}{}{}/", hostname, port, req.origin.uri)
}

fn main() {
    let mut server = Nickel::new();

    let sqlite = SqliteMiddleware::new(&"players.db", 5, Box::new(NopErrorHandler)).unwrap();
    migrate(&sqlite.pool.get().unwrap());

    server.utilize(sqlite);
    server.utilize(router!{
        get "/" => |_| {
            "Welcome to the Players API v0.1"
        }
        get "/players" => |req, mut res| {
            res.set(MediaType::Json);
            json::encode(&get_players(&req.db_conn())).unwrap()
        }
        post "/players" => |req, mut res| {
            res.set(MediaType::Json);
            if &ContentType::json() == req.origin.headers.get::<ContentType>().unwrap() {
                match req.json_as::<Player>() {
                    Ok(player) => {
                        match create_player(&req.db_conn(), player.name.to_string(), player.level) {
                            Ok(_) => {
                                res.set(Location(url_for_request(req) + &player.name));
                                (StatusCode::Created, json::encode(&player).unwrap())
                            }
                            Err(_) => (StatusCode::Conflict, "".to_string())
                        }
                    }
                    Err(_) => (StatusCode::BadRequest, "".to_string())

                }
            } else {
                (StatusCode::UnsupportedMediaType, "".to_string())
            }
        }
        get "/players/:player" => |req, mut res| {
            res.set(MediaType::Json);
            let player = req.param("player").unwrap();
            match get_player(&req.db_conn(), player) {
                Some(player) => (StatusCode::Ok, json::encode(&player).unwrap()),
                None => (StatusCode::NotFound, "".to_string())
            }
        }
        delete "/players/:player" => |req| {
            let player = req.param("player").unwrap();
            match delete_player(&req.db_conn(), player) {
                Ok(1) => (StatusCode::NoContent, ""),
                Ok(0) => (StatusCode::NotFound, ""),
                _ => (StatusCode::BadRequest, "")
            }
        }
    });
    server.listen("127.0.0.1:6767");
}
