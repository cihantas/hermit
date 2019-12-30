![Alt text](./logo.svg)

An IRC (RFC 1459) server written in Racket.

## Commands

| Command  | Supported | Note |
| -------- | :-------: | ---- |
| PASS     | ✅ | Not implemented.
| NICK     | ✅ |
| USER     | ✅ |
| SERVER   | ❌ |
| OPER     | ❓ |
| QUIT     | ✅ | Not implemented.
| SQUIT    | ❌ |
| JOIN     | ✅ |
| PART     | ✅ |
| MODE     | ✅ | Not implemented.
| TOPIC    | ✅ |
| NAMES    | ✅ | Not implemented.
| LIST     | ✅ | Not implemented.
| INVITE   | ❓ |
| KICK     | ✅ | Not implemented.
| VERSION  | ✅ | Parameter [<server>] not supported.
| STATS    | ❓ |
| LINKS    | ❌ |
| TIME     | ❌ |
| CONNECT  | ❌ |
| TRACE    | ❌ |
| ADMIN    | ✅ | Not implemented.
| INFO     | ✅ | Command not validated; Parameter [<server>] not supported.
| PRIVMSG  | ✅ |
| NOTICE   | ✅ | Not implemented.
| WHO      | ❓ |
| WHOIS    | ❓ |
| WHOWAS   | ❌ |
| KILL     | ❓ |
| PING     | ✅ |
| PONG     | ✅ | Not implemented.
| ERROR    | ❌ |
