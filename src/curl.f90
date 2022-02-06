module curl_fortran
    use json_module
    implicit none
    private

    public :: curl
    contains
    function curl(type, cmd) result(ret)
        use json_module
        implicit none
        character(len=256) :: cmd
        character(len=4) :: type
        character(len=256) :: ret
        type(json_file) :: json

        !!print *, "curl -X " // trim(type) // " " // trim(cmd) // " > tmp"
        call execute_command_line ("curl -X" // trim(type) // " " // trim(cmd) // " > tmp", wait=.true.)
        call json%initialize()
        call json%load(filename = "tmp")
        ret = json
    end function curl
end module curl_fortran