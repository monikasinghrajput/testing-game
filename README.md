# A Code Repo for Poker Web Server Written with Play Framework/React MVC Architecture.

## Overview
The Play Framework is designed to simplify the development of web applications 
using Java and Scala. It emphasizes developer productivity and supports REST-ful 
architectures, making it suitable for agile software development.
The Play Framework is licensed under the Apache License, Version 2.0.


## Tools Used
IntelliJ IDEA 2022
Scala Plugin
SBT 1.4.9
SBT Plugin - Play 2.8.4
JDK 11(e.g. Liberica JDK 11.)


## CI/CD Pipeline
Used GitHub Actions for continuous integration and deployment.

## Build Assembly
[poker-webapp-server] $ dist

## Run App
### Open sbt shell and execute below commands
[poker-webapp-server] $ docker compose up

### Use Google Chrome Browser to connect serving apps/pages,
Dealer SPA - http://localhost:9000/admin
Dashboard SPA - http://localhost:9000/topper
Player 1 SPA - http://localhost:9000/player?ip=192.168.1.1
Player 2 SPA - http://localhost:9000/player?ip=192.168.1.2
Player 3 SPA - http://localhost:9000/player?ip=192.168.1.3
Player 4 SPA - http://localhost:9000/player?ip=192.168.1.4
Player 5 SPA - http://localhost:9000/player?ip=192.168.1.5
Player 6 SPA - http://localhost:9000/player?ip=192.168.1.6
Player 7 SPA - http://localhost:9000/player?ip=192.168.1.7
Player 8 SPA - http://localhost:9000/player?ip=192.168.1.8