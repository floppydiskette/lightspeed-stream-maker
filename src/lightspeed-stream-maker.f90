module lightspeed_stream_maker
  implicit none
  use :: curl, only: c_f_str_ptr
  private

  public :: cb
contains
  function cb (ptr, size, nmemb, client_data) bind(c)
    !! stolen from examples (:
    !! author: Philipp Engel
    !! license: ISC
    use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, c_ptr, c_size_t
    type(c_ptr),            intent(in), value :: ptr               !! C pointer to a chunk of the response.
    integer(kind=c_size_t), intent(in), value :: size              !! Always 1.
    integer(kind=c_size_t), intent(in), value :: nmemb             !! Size of the response chunk.
    type(c_ptr),            intent(in), value :: client_data       !! C pointer to argument passed by caller.
    integer(kind=c_size_t)                    :: response_callback !! Function return value.
    type(response_type), pointer              :: response          !! Stores response.
    character(len=:), allocatable             :: buf

    response_callback = int(0, kind=c_size_t)

    ! Are the passed C pointers associated?
    if (.not. c_associated(ptr)) return
    if (.not. c_associated(client_data)) return

    ! Convert C pointer to Fortran pointer.
    call c_f_pointer(client_data, response)
    if (.not. allocated(response%body)) response%body = ''

    ! Convert C pointer to Fortran allocatable character.
    call c_f_str_ptr(ptr, buf, nmemb)
    if (.not. allocated(buf)) return
    response%body = response%body // buf
    deallocate (buf)

    ! Return number of received bytes.
    response_callback = nmemb
  end function cb
end module lightspeed_stream_maker

program main
  use, intrinsic :: iso_c_binding
  use :: curl
  use :: callback_http
  implicit none

  !! we need to send all requests to https://demo.lightspeed.tv/
  character(len=*), parameter :: url = "https://demo.lightspeed.tv/"
  type(c_ptr) :: curl_ptr
  integer :: curl_code
  type(response_type), target :: response

  !! initialize curl
  curl_ptr = curl_easy_init()
  if (.not. c_associated(curl_ptr)) then
    print *, "curl_easy_init() failed, fuck!" !! sorry for swearing don't tell my mom please
    stop
  end if

  !! set default curl options
  curl_code = curl_easy_setopt(curl_ptr, CURLOPT_DEFAULT_PROTOCOL, "https")
  curl_code = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT, 10)
  curl_code = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT, 10)
  curl_code = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION, .true.)

  !! set callback function
  curl_code = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION, after_login)
  curl_code = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA, c_loc(response))

  !! variables
  character(128) :: username
  character(128) :: password
  character(128) :: email
  character(1) :: answer

  !! prompt user for username and password
  print *, "please enter ur email: "
  read(*, *) email
  print *, "please enter ur password: "
  read(*, *) password
  print *, "please enter ur desired username: "
  read(*, *) username

  print *, "email: ", email
  print *, "password: ", password
  print *, "username: ", username
  print *, "is this correct? (y/n)"
  read(*, *) answer

  if (answer == "y") then
    !! get length of email and password
    integer :: email_len = len_trim(email)
    integer :: password_len = len_trim(password)
    integer :: len_required = email_len + password_len + len_trim(username) + 26 ! 26 is the length of the string {"email":"","password":""}

    !! string for req body
    character(len=len_required) :: req_body = "{\"email\":\"" // email // "\",\"password\":\"" // password // "\"}"
    !! string for req path
    character(len=18) :: req_path = "auth/session/login"
    !! string for req headers
    character(len=30) :: req_headers = "Content-Type: application/json"

    !! set curl options
    curl_code = curl_easy_setopt(curl_ptr, CURLOPT_URL, url // req_path)
    curl_code = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDS, req_body)
    curl_code = curl_easy_setopt(curl_ptr, CURLOPT_HTTPHEADER, c_loc(req_headers))

    !! send POST request
    curl_code = curl_easy_perform(curl_ptr)
    
  else
    print *, "bye!"
  end if