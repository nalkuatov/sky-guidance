sky-guidance
---

This contains a project I was asked to create as a part of an interview.

Some notes about libraries, techniques used throughout the project
---

  * `rio` package is used mainly as a replacement for basic `Prelude`,
  omitting its core `RIO` monad and other standartized stuff
  * Nested folders/modules/module-names are avoided to make navigation
  and examining more convenient
  * [Redis](https://redis.io/commands) is used as a backend for a caching capability of an app
  * `persistent`, `persistent-redis` libraries are used for records' serialization and querying

How to run
---

  * `docker-compose up -d redis`
  * `stack build`
  * `API_KEY=<your api key from openweathermap.org> stack exec sky-guidance-exe`
  * Change the configuration defaults, if neccessary, by editing `config.yaml`
  and passing in the `environment variables`

