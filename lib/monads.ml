module Lwt = struct
  include Lwt

  module Syntax = struct
    let ( let+ ) x f = map f x

    let ( let* ) = bind
  end
end

module Lwt_result = struct
  include Lwt_result

  module Syntax = struct
    let ( let+ ) x f = map f x

    let ( let* ) = bind
  end
end
