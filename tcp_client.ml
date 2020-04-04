open Core
open Lwt

let () =
  let server_address = Unix.Inet_addr.localhost in
  let port = 9000 in
  let addr = Unix.ADDR_INET (server_address, port) in

  let msg_server socket =
    let istream = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
    let ostream = Lwt_io.of_fd ~mode:Lwt_io.Output socket  in

    let rec user () =
      Lwt_io.read_line_opt Lwt_io.stdin >>= function
      | Some msg -> 
        let%lwt () = Lwt_io.write_line ostream msg in
        let%lwt () = Lwt_io.write_line Lwt_io.stdout ("Sent: " ^ msg) in
        user ()
      | None -> return ()
    in

    let rec server () =
      Lwt_io.read_line_opt istream >>= function
      | Some msg -> 
        let%lwt () = Lwt_io.write_line Lwt_io.stdout ("Rcvd: " ^ msg) in
        server ()
      | None -> 
        let %lwt () = Lwt_io.write_line Lwt_io.stdout "# Server Disconnected" in
        return ()
    in

    Lwt.choose [user (); server ()]
  in

  let open Lwt_unix in

  let server = 
    let sock = 
      socket PF_INET SOCK_STREAM 0
    in
    let%lwt () = connect sock addr in
    Lwt.finalize 
      (fun () -> msg_server sock) 
      (fun () -> close sock)
  in

  Lwt_main.run server
