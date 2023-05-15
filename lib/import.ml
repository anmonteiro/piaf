module Do_not_use_directly = struct end

include struct
  include Eio.Std
  include Monads.Bindings

  module Logging = struct
    include Logs

    let setup ~src ~doc =
      let src = Src.create src ~doc in
      (module (val src_log src) : LOG)
  end

  module Stream = Piaf_stream
  module Uri = Util.Uri
  module Backtrace = Util.Backtrace
end

module Logs = Do_not_use_directly
[@@deprecated "Accessing this module directly is deprecated"]
