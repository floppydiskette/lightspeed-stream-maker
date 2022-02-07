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
				write(*, 'a', advance='no') achar(27) // '[31m'
			case ('GRN')
				write(*, 'a', advance='no') achar(27) // '[32m'
			case ('YEL')
				write(*, 'a', advance='no') achar(27) // '[33m'
			case ('BLU')
				write(*, 'a', advance='no') achar(27) // '[34m'
			case ('PUR')
				write(*, 'a', advance='no') achar(27) // '[35m'
			case ('CYA')
				write(*, 'a', advance='no') achar(27) // '[36m'
			case ('BLK') !! BLK here means BLANK, aka default text
				write(*, 'a', advance='no') achar(27) // '[0m'
			case ('   ') !! three spaces to reset (:
				write(*, 'a', advance='no') achar(27) // '[0m'
			case default
				write(*, 'a', advance='no') achar(27) // '[0m'
		end select
	end subroutine colour
end module colours 
