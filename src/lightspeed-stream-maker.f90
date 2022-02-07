module lightspeed_stream_maker
  use funny, only: curl2
  use json_module
  implicit none
  private

  public thingy
  contains
  subroutine thingy
    use funny, only: curl2
    use colours, only: colour
    use json_module
    implicit none
    integer :: answer, len
    character(len=:), allocatable :: req_body, req_path, req_headers, find, url
    character(len=:), allocatable :: token, ftl_id, tmp_one, tmp_two
    character(len=:), allocatable :: username, password, email, invite_code
    character(len=256) :: buf
    type(json_file) :: json
    logical :: found

    call json%initialize()

    !! we need to send all requests to https://demo.lightspeed.tv/
    url = 'https://demo.lightspeed.tv/'


    !! prompt user for username and password
    colour('CYA')
    print *, 'please enter email:'
    colour('BLK')
    read *, buf
    email = trim(buf)
    colour('CYA')
    print *, 'please enter password:'
    colour('BLK')
    read *, buf
    password = trim(buf)
    colour('CYA')
    print *, 'please enter desired username:'
    colour('BLK')
    read *, buf
    username = trim(buf)

    colour('GRN')
    print *, "email: " // trim(email)
    print *, "password: " // trim(password)
    print *, "username: " // trim(username)
    print *, "is this correct? (1 for yes, 0 for no)"
    colour('BLK')
    read(*, *) answer

    if (answer == 1) then
      !! get length of email and password

      !! string for req body 
      req_body = '{"email":"' // trim(email) // '","password":"' // trim(password) // '"}'
      !! string for req path
      req_path = "auth/session/login"
      !! string for req headers
      req_headers = "Content-Type: application/json"

      buf = "POST -H '" // req_headers // "' -d '" // req_body // "' '" // url // req_path // "'"
      !! make request
      colour('YEL')
      json = curl2(buf)

      call json%get('token', token, found)
      if (.not. found) then
        colour('RED')
        call json%get('type', tmp_one, found)
        if (found) then
          print *, "error: " // tmp_one
        else
          print *, "error: unknown"
        endif
        return
      end if

      req_body = '{"username":"' // trim(username) // '"}'
      req_path = url // "users/@me"

      buf = "PUT -H 'x-session-token:" // token // "' -H '" // req_headers // "' -d '" // req_body // "' '" // req_path // "'"

      !! make a new request
      json = curl2(buf)


      !! prompt user for invite code
      colour('CYA')
      print *, 'please enter invite code: '
      colour('BLK')
      read *, invite_code

      req_body = '{"invite":"' // trim(invite_code) // '"}'
      req_path = url // "streams"

      buf = "PUT -H 'x-session-token:" // token // "' -H '" // req_headers // "' -d '" // req_body // "' '" // req_path // "'"

      !! final request B)
      colour('YEL')
      json = curl2(buf)

      call json%get('ftl_id', ftl_id, found)
      if (.not. found) then
        colour('RED')
        call json%get('type', tmp_one, found)
        if (found) then
          print *, "error: " // tmp_one
        else
          print *, "error: unknown"
        endif
        return
      end if

      call json%get('token', token, found)
      if (.not. found) then
        colour('RED')
        call json%get('type', tmp_one, found)
        if (found) then
          print *, "error: " // tmp_one
        else
          print *, "error: unknown"
        endif
        return
      end if

      colour('GRN')
      print *, ""
      print *, "if all went well, your stream should now be available at https://web.demo.lightspeed.tv/" // username
      print *, "your stream key is: " // ftl_id // "-" // token

      colour('CYA')
      print *, "bye!"
      
    else
      colour('CYA')
      print *, "bye!"
    end if
  end subroutine thingy
end module lightspeed_stream_maker
