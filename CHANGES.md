0.2.0 2024-09-06
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
- Piaf.Response: Add `Body.sendfile` and `Response.sendfile`
  ([#124](https://github.com/anmonteiro/piaf/pull/124))
- Piaf.Config: Add `config.flush_headers_immediately`
  ([#125](https://github.com/anmonteiro/piaf/pull/125))
- Piaf.Server: Add `config.shutdown_timeout` to wait before shutting down the
  Piaf server ([#174](https://github.com/anmonteiro/piaf/pull/174))
- Websocket support ([#139](https://github.com/anmonteiro/piaf/pull/139))
- Multicore support ([#151](https://github.com/anmonteiro/piaf/pull/151))
- Allow binding to UNIX domain socket
  ([#161](https://github.com/anmonteiro/piaf/pull/161))
- Don't send invalid HTTP/2 headers
  ([#197](https://github.com/anmonteiro/piaf/pull/197))

0.1.0 2021-02-03
---------------

- Initial public release
