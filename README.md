# hscalendar

[![Build Status][status-png]][status]

This is `hscalendar`, a time tracking software written in HASKELL and ELM. It
consists in a couple of command line tools along a web frontend.

As for HASKELL, the code follows the best practices proposed by the FP complete
team, especially:

- [how to handle exceptions][exceptions]
- using the [ReaderT pattern][readert] and [RIO][rio] library

This project can demonstrate the use of the following libraries:
- [QuickCheck][quickcheck]
- [RIO][rio]
- [esqueleto][esqueleto]
- [haskell-to-elm][haskell-to-elm]
- [hspec][hspec]
- [optparse-applicative][optparse-applicative]
- [persistent][persistent]
- [refined][refined]
- [servant][servant]
- [yaml][yaml]

If you do find some of this code useful, please, let me know by sending me an
email at jeancharles.quillet@gmail.com

# Introduction

`hscalendar` base time unit is a half-day. It also maintains a list of projects.
A half-day can be either worked or not. In the former case:
- It requires to be linked to a project
- The times of arrival/departure should make sense:
    - a time of arrival should not be superior to a time of departure and vice
      versa
    - this rule is also enforced between the morning and the afternoon.

# Architecture

The project is split in different binaries:
- `hscalendar-cli`: a simple standalone command line tool
- `hscalendar-server`/`hscalendar-client`: [servant][servant] REST server/client
- `hscalendar-users`: command line tool to edit the user list allowed to connect
  to the web server

These programs share a common library located in the [src](src) directory.
The library contains the following sub-directories:
- [src/App](src/App): Contains the functions related to the applications: top
  level monad, configuration file, process launching, command line parsing, the
  servant API etc...
- [src/Db](src/Db): Contains the CRUD functions to edit and query the database
- [src/\*](src/): Other functions related to external libraries

# Frontend

The frontend is written en ELM and its sources are located in the
[frontend](frontend) directory. ELM datatypes are generated from the Haskell
datatypes using the library [haskell-to-elm][haskell-to-elm]. The executable in
charge of actually writing the ELM sources file is `elm-generator`.

Below are some screenshots of the frontend.

The month page showing a review of what happens during a full month:

<img src="https://raw.githubusercontent.com/jecaro/hscalendar/master/docs/month.png" width="300">

The day page where one can edit individual half-day:

<img src="https://raw.githubusercontent.com/jecaro/hscalendar/master/docs/day.png" width="300">

The project page, to add/rename/remove projects:

<img src="https://raw.githubusercontent.com/jecaro/hscalendar/master/docs/project.png" width="300">

# Deployment and CI

This project is built on each commit on the repository by [travis][status]. A
docker image is automatically sent on [docker hub][dockerhub] when the build and
tests succeed.

# How to try it ?

The easiest way is to pull the latest docker image:
```
docker pull jecaro/hscalendar-server:latest
```

Then start a shell in a new container:

```
docker run -ti --rm --network=host jecaro/hscalendar-server:latest bash
```

Initialize the default SQLite database:
```
./hscalendar-cli migrate
```

Add a user:
```
./hscalendar-users add myusername mysecretpassword
```

Start the server:
```
./hscalendar-server
```

That's it ! You can know go to the URL: http://localhost:8081/ and authenticate
yourself.

# Standalone command line tool

The command line tool is called `hscalendar-cli`.

## Configuration

The tool uses a configuration file located in
`~/.config/hscalendar/config.yml`. If it doesn't exists, it is created at first
launch.

Below is the default configuration file:
```yaml
_configDefaultHours:
  _defaultHoursForDayAfternoon:
    _defaultHoursLeft: 17:00:00
    _defaultHoursArrived: 13:30:00
  _defaultHoursForDayMorning:
    _defaultHoursLeft: 12:00:00
    _defaultHoursArrived: 08:20:00
_configDefaultOffice: Rennes
_configDbConfig:
  _dbConfigNbConnections: 1
  _dbConfigConnectionString: ~/.config/hscalendar/database.db
  _dbConfigBackend: Sqlite
```

It contains:
- the default values for departure and arrival time
- the default office
- the default database backend where all data is stored. The default database
  backend is a Sqlite file.

The database backend can be changed for a PostgreSql database. See below for an
example:

```yaml
_configDbConfig:
  _dbConfigNbConnections: 1
  _dbConfigConnectionString: postgres://mydbuser:mydbpass@localhost:5432/mydb
  _dbConfigBackend: Postgresql
```

The database configuration can be overriden by these environment variables:
`BACKEND` and `DATABASE_URL` which uses the same format as the configuration
file. It can be useful when running the service on a stateless docker
container.

## Synopsis

### Common options

A half-day is described by a date:
- `today`
- `yesterday`
- `tomorrow`
- `dd-mm-yy`
- `dd-mm`: current year is used
- `dd`: current month and year is used

and a time in the day either:
- `-m` for morning
- `-a` for afternoon

### Command line

Set a working half-day:
```
hscalendar-cli diary work DATE -m|-a [commands]
  commands:
    -p project set the project name
    -n note    set note
    -a 00:00   set time of arrival
    -l 00:00   set left time
    -o office  set the office: home|out|poool|rennes
```

Set a half-day as non-working:
- `pl`: Paid leave
- `fe`: Family event
- `rtte`: French kind of paid leave
- `rtts`: French kind of paid leave
- `ul`: Unpaid leave
- `ph`: Public holiday
- `pt`: Part-time
```
hscalendar-cli diary off DATE -m|-a pl|fe|rtte|rtts|ul|ph|pt
```

Remove a half-day:
```
hscalendar-cli diary rm DATE -m|-a
```

Display a half-day:
```
hscalendar-cli diary display DATE -m|-a
```

Display a full week. The week definition can be either:
- `current`: for current week
- `ww`: the week number of current year
- `yyyy-ww`: the year and the week number
```
hscalendar-cli diary week WEEK
```

Launch an editor to edit a working half-day:
```
hscalendar-cli diary edit DATE -m|-a
```

List all the projects:
```
hscalendar-cli project list
```

Remove a project:
```
hscalendar-cli project rm NAME
```

Add a project:
```
hscalendar-cli project add NAME
```

Rename a project:
```
hscalendar-cli project rename OLDNAME NEWNAME
```

# Client/server mode

`hscalendar` can also operate in a client/server mode. The server exposes a
REST API using [servant](servant).

## Server

The server uses the same configuration file and environment variables as the
command line tool.

`hscalendar-server [-v] [-p PORT]`

The command line options are:
- `-v` Toggle the logs in verbose mode
- `-p PORT` Override the default port number: 8081

## Client

The client uses exactly the same options as the standalone tool. Only its first
argument is different. It consists in the URL used to contact the server, for
example:
- `http://myuser:mypassword@localhost:8081` for http
- `https://myuser:mypassword@droplet:443` for joining the server over https

# Roadmap

- ~~Command line tool~~
- ~~Unit tests with [hspec][hspec]~~
- ~~[QuickCheck][quickcheck] property based testing~~
- ~~CI with [travis][status]~~
- ~~web server with [servant][servant]~~
- ~~deployement on [heroku][heroku]~~
- ~~dockerize the app~~
- ~~deploy on [digital ocean][digitalocean]~~
- ~~web frontend with [Elm][elm]~~


[digitalocean]: https://www.digitalocean.com/
[dockerhub]: https://hub.docker.com/r/jecaro/hscalendar-server/tags
[elm]: https://elm-lang.org/
[esqueleto]: https://github.com/bitemyapp/esqueleto
[exceptions]: https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
[haskell-gi]: https://github.com/haskell-gi/haskell-gi
[haskell-to-elm]: https://github.com/folq/haskell-to-elm
[heroku]: https://www.heroku.com/
[hspec]: https://github.com/hspec/hspec
[optparse-applicative]: https://github.com/pcapriotti/optparse-applicative
[persistent]: https://github.com/yesodweb/persistent
[quickcheck]: https://github.com/nick8325/quickcheck
[readert]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
[refined]: https://github.com/nikita-volkov/refined
[rio]: https://github.com/commercialhaskell/rio
[servant]: https://github.com/haskell-servant/servant
[status-png]: https://travis-ci.org/jecaro/hscalendar.svg?branch=master
[status]: https://travis-ci.org/jecaro/hscalendar?branch=master
[yaml]: https://github.com/snoyberg/yaml
