module colours
    implicit none
    private
    public colour
    contains
        subroutine colour(c)
		implicit none
		character(len=3) :: c
		select case (c)
			case ('RED')
				print *, achar(27) // '[31m'
			case ('BLK') !! BLK here means BLANK, aka default text
				print *, achar(27) // '[0m'
			case ('   ') !! three spaces to reset (:
				print *, achar(27) // '[0m'
			case default
				print *, achar(27) // '[0m'
		end select
	end subroutine colour
end module colours 
