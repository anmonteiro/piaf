Unreleased
---------------
- Improve certificate checking and authentication
  ([#93](https://github.com/anmonteiro/piaf/pull/93)) -
  [@Firgeis](https://github.com/Firgeis)
- Check certificate SAN IP address when appropriate
  ([#96](https://github.com/anmonteiro/piaf/pull/96)) -
  [@Firgeis](https://github.com/Firgeis)
- Close the file descriptor when failing to open a connection
  ([#97](https://github.com/anmonteiro/piaf/pull/97)) -
  [@EduardoRFS](https://github.com/EduardoRFS)
- Yield to other threads when reading a message body. This improves fairness
  for large message bodies
  ([#100](https://github.com/anmonteiro/piaf/pull/100))
- Add error handling to `Response.of_file`
  ([#103](https://github.com/anmonteiro/piaf/pull/103))
- Add `Client.send` which sends a `Request.t`
  ([#110](https://github.com/anmonteiro/piaf/pull/110))
- openssl: set the client verify callback
  ([#112](https://github.com/anmonteiro/piaf/pull/112))
- Piaf.Response: add `or_internal_error`
  ([#120](https://github.com/anmonteiro/piaf/pull/120))
- Piaf.Server: prefer `Lwt.dont_wait` to the global exception hook
  ([#123](https://github.com/anmonteiro/piaf/pull/123))
- Piaf.Response: Add `Body.sendfile` and `Response.sendfile`
  ([#124](https://github.com/anmonteiro/piaf/pull/123))
- Piaf.Config: Add `config.flush_headers_immediately`
  ([#125](https://github.com/anmonteiro/piaf/pull/125))

0.1.0 2021-02-03
--------------

- Initial public release
