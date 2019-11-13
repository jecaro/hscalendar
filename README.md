# hscalendar

[![Build Status][status-png]][status]

This is `hscalendar`, a time tracking software I develop to learn the HASKELL
programming language. It uses the best practices proposed by the FP complete
team:

- [how to handle exceptions][exceptions]
- using the [ReaderT pattern][readert] and [RIO][rio] library

It consists in a unit tested API along a command line tool. The command line
tool is configured by with a configuration file located in
`~/.config/hscalendar/config`. Its interface allows to update a database stored
in a SQLite file.

This simple program can demonstrate the use of the following libraries:
- [QuickCheck][quickcheck]
- [RIO][rio]
- [esqueleto][esqueleto]
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

# Command line tool

The command line tool is called `hscalendar-cli`.

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
hscalendar-cli diary work date -m|-a [commands]
  commands:
    -p project   set the project name
    -n note      set note
    -a 00:00     set time of arrival
    -l 00:00     set left time
    -o office    set the office: home|out|poool|rennes
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
hscalendar-cli diary holiday date -m|-a [pl|fe|rtte|rtts|ul|ph|pt]
```

Remove a half-day:
```
hscalendar-cli diary rm date -m|-a
```

Display a half-day:
```
hscalendar-cli diary display date -m|-a
```

Launch an editor to edit a working half-day:
```
hscalendar-cli diary edit date -m|-a
```

List all the projects:
```
hscalendar-cli project list
```

Remove a project:
```
hscalendar-cli project rm project
```

Add a project:
```
hscalendar-cli project add project
```

Rename a project:
```
hscalendar-cli project rename project1 project2
```

# Roadmap

- ~~Command line tool~~
- ~~Unit tests with [hspec][hspec]~~
- ~~[QuickCheck][quickcheck] property based testing~~
- ~~CI with [travis][status]~~
- ~~web server with [servant][servant]~~
- ~~deployement on [heroku][heroku]~~
- web frontend: technology not choosen yet
- gtk frontend with [haskell-gi][haskell-gi]


[esqueleto]: https://github.com/bitemyapp/esqueleto
[exceptions]: https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
[haskell-gi]: https://github.com/haskell-gi/haskell-gi
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
