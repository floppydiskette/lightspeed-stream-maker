module lightspeed_stream_maker
  use curl_fortran, only: curl
  implicit none
  private

  public :: thingy
  contains
  subroutine thingy
    implicit none
    integer :: answer, len
    character(len=:), allocatable :: req_body, req_path, req_headers, find, url
    character(len=:), allocatable :: token, ftl_id
    character(len=256) :: username, password, email, invite_code, cmd, response

    !! we need to send all requests to https://demo.lightspeed.tv/
    url = 'https://demo.lightspeed.tv/'


    !! prompt user for username and password
    print *, 'please enter email:'
    read *, email
    print *, 'please enter password:'
    read *, password
    print *, 'please enter desired username:'
    read *, username

    print *, "email: " // trim(email)
    print *, "password: " // trim(password)
    print *, "username: " // trim(username)
    print *, "is this correct? (1 for yes, 0 for no)"
    read(*, *) answer

    if (answer == 1) then
      !! get length of email and password

      !! string for req body 
      req_body = '{"email":"' // trim(email) // '","password":"' // trim(password) // '"}'
      !! string for req path
      req_path = "auth/session/login"
      !! string for req headers
      req_headers = "Content-Type: application/json"

      cmd = "-H '" // req_headers // "' -d '" // req_body // "' '" // url // req_path // "'"
      !!print *, cmd
      !! make request
      response = curl('POST', cmd)
      print *, response

      !! get everything after token":"
      find = '"token":"'
      if (index(response, find) == 0) then
        print *, "error: could not find token"
        print *, "reason: " // response
        return
      else
        !! get length of response
        len = len_trim(response)
        response = response(index(response, find) + len_trim(find) : len)
      end if

      !! get everything before ","name":"
      find = '" "name":"'
      if (index(response, find) == 0) then
        print *, "error: could not find name"
        return
      else
        !! get length of response
        len = len_trim(response)
        response = response(1 : index(response, find) - 1)
      end if

      token = response

      req_body = '{"username":"' // trim(username) // '"}'
      req_path = url // "users/@me"

      cmd = "-H 'x-session-token:" // token // "' -H '" // req_headers // "' -d '" // req_body // "' '" // req_path // "'"

      !! make a new request
      response = curl('PUT ', cmd)


      !! prompt user for invite code
      print *, 'please enter invite code: '
      read *, invite_code

      req_body = '{"invite":"' // trim(invite_code) // '"}'
      req_path = url // "streams"

      cmd = "-H 'x-session-token:" // token // "' -H '" // req_headers // "' -d '" // req_body // "' '" // req_path // "'"

      !! final request B)
      response = curl('PUT ', cmd)

      !! get everything after "ftl_id":"
      find = '"ftl_id":"'
      if (index(response, find) == 0) then
        print *, "error: could not find ftl_id"
        return
      else
        !! get length of response
        len = len_trim(response)
        ftl_id = response(index(response, find) + len_trim(find) : len)
      end if

      !! get everything before ","token":"
      find = '" "token":"'
      if (index(ftl_id, find) == 0) then
        print *, "error: could not find token"
        return
      else
        !! get length of response
        len = len_trim(ftl_id)
        ftl_id = ftl_id(1 : index(ftl_id, find) - 1)
      end if

      !! get everything after "token":"
      find = '" "token":"'
      if (index(response, find) == 0) then
        print *, "error: could not find token"
        return
      else
        !! get length of response
        len = len_trim(response)
        token = response(index(response, find) + len_trim(find) : len)
      end if

      !! get everything before "}
      find = '"}'
      if (index(token, find) == 0) then
        print *, "error: could not find }"
        return
      else
        !! get length of response
        len = len_trim(token)
        token = token(1 : index(token, find) - 1)
      end if

      !! print *, "response: ", response
      print *, "if all went well, your stream should now be available at https://web.demo.lightspeed.tv/" // username
      print *, "your stream key is: " // ftl_id // "-" // token

      print *, "bye!"
      
    else
      print *, "bye!"
    end if
  end subroutine thingy
end module lightspeed_stream_maker