# HaskGuard:

## Overview

HaskGuard is a SOCKS5 server written in Haskell.
It supports both IPv4 and IPv6,  HaskGuard implements several features from relevant RFCs, including various authentication methods and command types, making it a versatile tool for secure and efficient proxying.
Features

- Dual-Stack Support: Fully supports both IPv4 and IPv6. 
- SOCKS5 Protocol: Comprehensive implementation of the SOCKS5 protocol as per RFC 1928.
- Authentication: no authentication is implemented yet.
- Commands: Supports CONNECT only yet (no support for BIND, and UDP ASSOCIATE).
- Concurrency: Manages multiple simultaneous connections using Haskell's lightweight concurrency.
- Logging: Detailed and configurable logging for monitoring and debugging.
- Configuration: Easily configurable via a INI file (see config.ini to have a full description of all available options).

## Installation

How to build the project :
```
$ stack build
```

How to tun the project
```
$ stack run
```

How to run all tests
```
$ stack test
```