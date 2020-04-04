open Core
open Lwt

let () =
  let server_address = Unix.Inet_addr.localhost in
  let port = 9000 in
  let addr = Unix.ADDR_INET (server_address, port) in
  let buffer_size = 16 in

  let msg_server socket server_addr =
    let buffer = Bytes.create buffer_size in

    let rec user () =
      Lwt_io.read_line_opt Lwt_io.stdin >>= function
      | Some msg -> 
        let%lwt _ = Lwt_unix.sendto socket (Bytes.of_string msg) 0 (String.length msg) [] server_addr in
        let%lwt () = Lwt_io.write_line Lwt_io.stdout ("Sent: " ^ msg) in
        user ()
      | None -> return ()
    in

    let rec server () = 
      Lwt_unix.recvfrom socket buffer 0 buffer_size [] >>= fun (len, _) ->
      if (len > 0) then
        let msg = Bytes.sub ~pos:0 ~len:len buffer |> Bytes.to_string |> String.strip ~drop:(fun x -> Char.equal x '\n') in
        let%lwt () = Lwt_io.write_line Lwt_io.stdout ("Rcvd: " ^ msg) in
        server ()
      else
        return ()
    in

    Lwt.choose [user (); server ()]
  in

  let open Lwt_unix in

  let server = 
    let sock = 
      socket PF_INET SOCK_DGRAM 0
    in
    let%lwt () = connect sock addr in
    Lwt.finalize 
      (fun () -> msg_server sock addr) 
      (fun () -> close sock)
  in

  Lwt_main.run server
