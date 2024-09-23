open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Soup

let credentials_file = "user.txt"
let login_url = "https://www.gradescope.com/login"
let global_cookies = ref []

let read_user_credentials =
  let open Lwt_io in
  with_file ~mode:Input credentials_file (fun ic ->
      read_lines ic |> Lwt_stream.to_list
      >|= List.fold_left
            (fun acc line ->
              match String.split_on_char '=' line with
              | [ key; value ] -> (key, value) :: acc
              | _ -> acc)
            [])

let get_email_password credentials =
  try
    let email = List.assoc "email" credentials in
    let password = List.assoc "password" credentials in
    (email, password)
  with Not_found ->
    failwith
      ("Email or password not found in credentials file (" ^ credentials_file
     ^ ")")

let update_global_cookies headers =
  global_cookies := Cohttp.Header.get_multi headers "set-cookie"

let fetch_login_page url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  update_global_cookies (Response.headers resp);
  let soup = Soup.parse body_str in
  Lwt.return soup

let extract_form_and_action soup =
  (* Find the form element *)
  let form = Soup.select_one "form" soup in
  match form with
  | None -> failwith "Form not found"
  | Some f ->
      (* Get action URL, defaults to the current page if not found *)
      let action =
        match Soup.attribute "action" f with
        | None -> "/"
        | Some action -> action
      in
      (* Find the input fields *)
      let inputs = Soup.select "input" f |> Soup.to_list in
      let params =
        List.fold_left
          (fun acc input ->
            match
              (Soup.attribute "name" input, Soup.attribute "value" input)
            with
            | Some name, Some value -> (name, value) :: acc
            | Some name, None -> (name, "") :: acc
            | _ -> acc)
          [] inputs
      in
      (action, params)

let follow_redirect url =
  let headers =
    Header.init () |> fun h -> Header.add_multi h "Cookie" !global_cookies
  in
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  let status = Response.status resp in
  if status = `OK then Lwt.return body_str
  else
    Lwt.fail_with ("Unexpected response status: " ^ Code.string_of_status status)

let handle_login_redirect login_response_headers =
  match Cohttp.Header.get login_response_headers "location" with
  | Some redirect_url ->
      Lwt_io.printf "Logged in. Redirecting to: %s\n" redirect_url >>= fun () ->
      follow_redirect redirect_url
  | None -> Lwt.fail_with "No redirect location found"

let submit_login_form url form_action params email password =
  (* Replace the username and password fields *)
  let params =
    List.map
      (fun (k, v) ->
        if k = "session[email]" then (k, [ email ])
        else if k = "session[password]" then (k, [ password ])
        else (k, [ v ]))
      params
  in

  (* Encode the form data *)
  let body = Uri.encoded_of_query params in
  let headers =
    Header.init () |> fun h ->
    Header.add h "Content-Type" "application/x-www-form-urlencoded" |> fun h ->
    Header.add h "User-Agent"
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, \
       like Gecko) Chrome/128.0.0.0 Safari/537.36 Edg/128.0.0.0"
    |> fun h ->
    Header.add h "Origin" "https://www.gradescope.com" |> fun h ->
    Header.add h "Referer" "https://www.gradescope.com/login" |> fun h ->
    Header.add h "Accept"
      ("text/html," ^ "application/xhtml+xml," ^ "application/xml;q=0.9,"
     ^ "image/avif," ^ "image/webp," ^ "image/apng," ^ "*/*;q=0.8,"
     ^ "application/signed-exchange;v=b3;q=0.7")
    |> fun h ->
    Header.add h "Accept-Encoding" "gzip, deflate, br, zstd" |> fun h ->
    Header.add h "Accept-Language"
      "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"
    |> fun h ->
    Header.add h "Upgrade-Insecure-Requests" "1" |> fun h ->
    Header.add_multi h "Cookie" !global_cookies
  in
  let uri = Uri.with_path (Uri.of_string url) form_action in

  (* Send the POST request with form data *)
  Client.post ~body:(Cohttp_lwt.Body.of_string body) ~headers uri
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  let status = Response.status resp in

  if status = `Found || status = `Moved_permanently then (
    update_global_cookies (Response.headers resp);
    handle_login_redirect (Response.headers resp))
  else Lwt.return body_str

let login email password =
  fetch_login_page login_url >>= fun soup ->
  let form_action, params = extract_form_and_action soup in
  submit_login_form login_url form_action params email password

type course_info = {
  shortname : string;
  fullname : string;
  assignments : string;
  link : string;
}

(* html_body: contents of https://www.gradescope.com/account after logged in *)
let extract_courses html_body =
  let soup = Soup.parse html_body in
  let course_list =
    soup $$ "div.courseList--coursesForTerm a.courseBox" |> Soup.to_list
  in
  List.map
    (fun course_node ->
      let shortname = course_node $ "h3.courseBox--shortname" |> R.leaf_text in
      let fullname = course_node $ "div.courseBox--name" |> R.leaf_text in
      let assignments =
        course_node $ "div.courseBox--assignments" |> R.leaf_text
      in
      let link = Soup.attribute "href" course_node |> Option.get in
      { shortname; fullname; assignments; link })
    course_list

(* Structure to hold assignment information *)
type assignment_info = {
  name : string;
  status : string;
  release_date : string;
  due_date : string;
  late_due_date : string option;
}

let extract_assignments html_body =
  let soup = Soup.parse html_body in

  let assignments_table =
    soup $ "table#assignments-student-table" $$ "tbody tr" |> Soup.to_list
  in

  (* Extract assignment details from each row *)
  List.map
    (fun row ->
      (* Extract assignment name, which could be a button or link *)
      let name =
        let button = Soup.select_one "button.js-submitAssignment" row
        and link = Soup.select_one "a" row in
        (match (button, link) with
        | Some button, _ -> Soup.leaf_text button
        | None, Some link -> Soup.leaf_text link
        | None, None -> None)
        |> Option.value ~default:"Unknown assignment"
      in
      (* Extract status *)
      let status =
        row $ "td.submissionStatus div.submissionStatus--text" |> R.leaf_text
      in
      (* Extract release date *)
      let release_date =
        row $ "time.submissionTimeChart--releaseDate" |> R.leaf_text
      in
      (* Extract due date *)
      let due_date = row $ "time.submissionTimeChart--dueDate" |> R.leaf_text in
      (* Late due date is optional, so we handle it carefully *)
      let late_due_date =
        match Soup.select_one "time[aria-label^='Late Due Date']" row with
        | None -> None
        | Some late_time -> Soup.leaf_text late_time
      in
      { name; status; release_date; due_date; late_due_date })
    assignments_table

let assignments_str assignments =
  List.fold_left
    (fun acc assignment ->
      acc
      ^ Printf.sprintf
          "  Assignment: %s\n    Status: %s\n    Released: %s\n    Due: %s\n%s"
          assignment.name assignment.status assignment.release_date
          assignment.due_date
          (match assignment.late_due_date with
          | Some late -> Printf.sprintf "    Late Due: %s\n" late
          | None -> ""))
    "" assignments

let fetch_course_assignments course =
  let url = "https://www.gradescope.com" ^ course.link in
  let headers =
    Header.init () |> fun h -> Header.add_multi h "Cookie" !global_cookies
  in
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  update_global_cookies (Response.headers resp);
  let assignments = extract_assignments body_str in
  Lwt_io.print (course.shortname ^ ":\n" ^ assignments_str assignments)
  >>= fun () -> Lwt.return ()

let fetch_all_courses courses =
  let str =
    List.fold_left
      (fun acc course ->
        acc
        ^ Printf.sprintf "  %s (url=%s): %s \n" course.shortname
            (String.sub course.link 9 (String.length course.link - 9))
            course.assignments)
      "" courses
  in
  Lwt_io.printf "Course list:\n%s" str >>= fun () ->
  Lwt_list.iter_p fetch_course_assignments courses >>= fun () -> Lwt.return ()

let main () =
  Lwt_main.run
    ( read_user_credentials >>= fun credentials ->
      let email, password = get_email_password credentials in
      login email password >>= fun body ->
      fetch_all_courses (extract_courses body) >>= fun () -> Lwt.return () )
