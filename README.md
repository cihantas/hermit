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
| VERSION  | ✅ | Parameter [\<server\>] not supported.
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

## Replies 
| Code | Name                | Supported | Note |
| ---- | --------------------| :-------: | ---- |
| 251  | RPL_LUSERCLIENT     | ✅ | Not implemented.
| 252  | RPL_LUSEROP         | ✅ | Not implemented.
| 253  | RPL_LUSERUNKNOWN    | ✅ | Not implemented.
| 254  | RPL_LUSERCHANNELS   | ✅ | Not implemented.
| 255  | RPL_LUSERME         | ✅ | Not implemented.