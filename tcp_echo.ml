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

  let id_key = Lwt.new_key () in

  let format_msg msg =
    let thread_id = match Lwt.get id_key with
      | Some id -> "Thread " ^ Int.to_string id ^ ": "
      | None -> "Main Thread: "
    in
    thread_id ^ msg in

  let rec echo_server istream ostream () =
    Lwt_io.read_line_opt istream >>= function
    | Some msg -> 
      let%lwt () = Lwt_io.write_line Lwt_io.stdout (format_msg msg) in
      let%lwt () = Lwt_io.write_line ostream msg in
      echo_server istream ostream ()
    | None -> Lwt_io.write_line Lwt_io.stdout (format_msg "Client Disconnected")
  in

  let accept_connection conn =
    let fd, _= conn in
    let istream = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let ostream = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    let%lwt () = Lwt_io.write_line Lwt_io.stdout (format_msg "Client Connected") in
    Lwt.finalize 
      (fun () -> Lwt.choose [echo_server istream ostream (); sigint_waiter])
      (fun () -> Lwt_unix.close fd)
  in

  let create_server socket =
    let rec serve () = 
      let%lwt () = Lwt_unix.accept socket >>= accept_connection in
      serve ()
    in serve
  in

  let open Lwt_unix in

  let server = 
    let sock = socket PF_INET SOCK_STREAM 0 in
    let () = setsockopt sock SO_REUSEADDR true in
    let%lwt () = bind sock addr in
    let () = listen sock 8 in
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
