open Core
open Lwt

let sigint_waiter =
  let w,u = Lwt.wait () in
  let sig_handler = Lwt_unix.on_signal
      (Signal.to_system_int Signal.int)
      (fun _ ->
         Lwt.wakeup u ())
  in 
  w >>= fun () -> return (Lwt_unix.disable_signal_handler sig_handler)

let () =
  let listen_address = Unix.Inet_addr.localhost in
  let port = 9000 in
  let addr = Unix.ADDR_INET (listen_address, port) in
  let buffer_size = 16 in

  let id_key = Lwt.new_key () in

  let format_msg msg =
    let thread_id = match Lwt.get id_key with
      | Some id -> "Thread " ^ Int.to_string id ^ ": "
      | None -> "Main Thread: "
    in
    thread_id ^ msg in

  let echo_server socket buffer len addr =
    let msg = Bytes.sub ~pos:0 ~len:len buffer |> Bytes.to_string |> String.strip ~drop:(fun x -> Char.equal x '\n') in
    let%lwt () = Lwt_io.write_line Lwt_io.stdout (format_msg msg) in
    let%lwt _ = Lwt_unix.sendto socket buffer 0 len [] addr in
    return ()
  in

  let create_server socket =
    let buffer = Bytes.create buffer_size in
    let rec serve () = 
      let%lwt () = Lwt_unix.recvfrom socket buffer 0 buffer_size [] >>= 
        fun (len, addr) -> echo_server socket buffer len addr in
      serve ()
    in serve
  in

  let open Lwt_unix in

  let server = 
    let sock = socket PF_INET SOCK_DGRAM 0 in
    let%lwt () = bind sock addr in
    let%lwt () = Lwt_io.write_line Lwt_io.stdout 
        (format_msg (sprintf "Listening on %s port %d ..." (Unix.Inet_addr.to_string listen_address) port)) in
    Lwt.finalize 
      (fun () ->
         Lwt.choose [Lwt.join (List.init 10 ~f:(fun i -> 
             Lwt.with_value id_key (Some i) ( fun () ->
                 create_server sock ())));
            sigint_waiter])
      (fun () -> close sock)
  in Lwt_main.run server
