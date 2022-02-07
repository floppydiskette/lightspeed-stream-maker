module funny
    use json_module
    implicit none
    private

    public curl2
    contains
        function curl2(cmd) result(json)
            use json_module
            implicit none
            character(len=256) :: cmd
            type(json_file) :: json

            !!print *, "curl -X" // trim(cmd) // " > tmp"
            call execute_command_line ("curl -X" // trim(cmd) // " > tmp", wait=.true.)
            call json%initialize()
            call json%load(filename = "tmp")

            !! delete tmp
            open(unit=10, file="tmp", status="unknown")
            close(unit=10, status="delete")
        end function curl2
end module funny