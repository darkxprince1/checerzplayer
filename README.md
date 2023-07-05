Subroutine player1 (Board,Move,Player)
Implicit None
Integer,intent(in)::board(8,8),Player
integer,intent(out):: Move(2,2)

			
  ! Variables for tracking the best move
  integer :: bestMoveRow, bestMoveCol
  integer :: bestMoveScore
  logical :: foundBestMove
	  integer :: i, j
	  integer :: score 
	  ! Internal subroutines

  ! Initialize variables
  bestMoveScore = -1
  foundBestMove = .false.

  ! Iterate over each position on the board
 
	      ! Iterate over each position on the board
  do i = 1, 8
    do j = 1, 8
      ! Check if the current position is a valid move for the player
      call IsValidMove(board, player, i, j) 
        ! Evaluate the desirability of the move
        call EvaluateMove(board, player, i, j, score)

        ! Update the best move if the current move has a higher score
        if (score > bestMoveScore) then
          bestMoveRow = i
          bestMoveCol = j
          bestMoveScore = score
          foundBestMove = .true.
        end if
      
   		   end do
  end do
 

  ! Make the best move if a valid move was found
  if (foundBestMove) then
    call MakeMove(board, player, bestMoveRow, bestMoveCol)
  end if






 function IsValidMove(board, player, row, col) result(valid)
  implicit none
  integer, dimension(8, 8), intent(in) :: board
  integer, intent(in) :: player
  integer, intent(in) :: row, col
  logical :: valid

  ! Check if the position is within the board boundaries
  if (row < 1 .or. row > 8 .or. col < 1 .or. col > 8) then
    valid = .false.
    return
  end if

  ! Check if the position is empty
  if (board(row, col) /= 0) then
    valid = .false.
    return
  end if

  ! Check if the player is moving a normal checker or a king checker
   if (player == 1) then
    ! Check if the piece is a normal checker
     if (board(row, col) == 1) then
        ! Check if the player can move diagonally down
         if (row < 8 .and. (col > 1 .or. col < 8)) then
            ! Check if the position below and to the left/right is occupied by the opponent's piece
            ! Check if the position below and to the left/right is empty
             if (board(row + 1, col - 1) == 0 .or. board(row + 1, col + 1) == 0) then
                valid = .true.
            else if (board(row + 1, col - 1) == 2 .or. board(row + 1, col + 1) == 2) then
                ! Check if the back of the opponent's piece is open
                if (row < 7 .and. (col > 2 .or. col < 7)) then
                    if (board(row + 2, col - 2) == 0 .or. board(row + 2, col + 2) == 0) then
                        valid = .true.
                        return
                    end if 
                else
                    valid = .true.
                    return
                end if 
            end if	
        end if	
    		

    else if (board(row, col) == 11) then
        ! Check if the player can move diagonally down
         if (row < 8 .and. (col > 1 .or. col < 8)) then
            ! Check if the position below and to the left/right is occupied by the opponent's piece
            ! Check if the position below and to the left/right is empty
            if (board(row + 1, col - 1) == 0 .or. board(row + 1, col + 1) == 0) then
                valid = .true.
             else if (board(row + 1, col - 1) == 2 .or. board(row + 1, col + 1) == 2) then
                ! Check if the back of the opponent's piece is open
                if (row < 7 .and. (col > 2 .or. col < 7)) then
                    if (board(row + 2, col - 2) == 0 .or. board(row + 2, col + 2) == 0) then
                        valid = .true.
                        return
                    end if 
                else
                    valid = .true.
                    return
                end if 
            end if	 
        
			 end if	  
			  
			
        ! Check if the player can move diagonally up
        if (row > 1 .and. (col > 1 .or. col < 8)) then
            ! Check if the position above and to the left/right is occupied by the opponent's piece
            ! Check if the position above and to the left/right is empty
            if (board(row - 1, col - 1) == 0 .or. board(row - 1, col + 1) == 0) then
                valid = .true.
            else if (board(row - 1, col - 1) == 2 .or. board(row - 1, col + 1) == 2) then
                ! Check if the back of the opponent's piece is open
                 if (row > 2 .and. (col > 2 .or. col < 7)) then
                     if (board(row - 2, col - 2) == 0 .or. board(row - 2, col + 2) == 0) then
                        valid = .true.
                        return
                    end if 
                else			
                    valid = .true.
                    return
                end if 
            end if 
        end if 
			  
		   end if

       
       else if (player == 2) then
    ! Check if the piece is a normal checker
    if (board(row, col) == 2) then
        ! Check if the player can move diagonally up
        if (row > 1 .and. (col > 1 .or. col < 8)) then
            ! Check if the position above and to the left/right is occupied by the opponent's piece
            ! Check if the position above and to the left/right is empty
            if (board(row - 1, col - 1) == 0 .or. board(row - 1, col + 1) == 0) then
                valid = .true.
            else if (board(row - 1, col - 1) == 1 .or. board(row - 1, col + 1) == 1) then
                ! Check if the back of the opponent's piece is open
                if (row > 2 .and. (col > 2 .or. col < 7)) then
                    if (board(row - 2, col - 2) == 0 .or. board(row - 2, col + 2) == 0) then
                        valid = .true.
                        return
                    end if
                else
                    valid = .true.
                    return
                end if
            end if
        end if
    
 		else if (board(row, col) == 22) then
    ! Check if the player can move diagonally down
    if (row < 8 .and. (col > 1 .or. col < 8)) then
        ! Check if the position below and to the left/right is occupied by the opponent's piece
        ! Check if the position below and to the left/right is empty
        if (board(row + 1, col - 1) == 0 .or. board(row + 1, col + 1) == 0) then
            valid = .true.
        else if (board(row + 1, col - 1) == 1 .or. board(row + 1, col + 1) == 1) then
            ! Check if the back of the opponent's piece is open
            if (row < 7 .and. (col > 2 .or. col < 7)) then
                if (board(row + 2, col - 2) == 0 .or. board(row + 2, col + 2) == 0) then
                    valid = .true.
                    return
                end if
            else
                valid = .true.
                return
            end if
        end if
   		 end if

    ! Check if the player can move diagonally up
     if (row > 1 .and. (col > 1 .or. col < 8)) then
        ! Check if the position above and to the left/right is occupied by the opponent's piece
        ! Check if the position above and to the left/right is empty
        if (board(row - 1, col - 1) == 0 .or. board(row - 1, col + 1) == 0) then
            valid = .true.
        else if (board(row - 1, col - 1) == 1 .or. board(row - 1, col + 1) == 1) then
            ! Check if the back of the opponent's piece is open
            if (row > 2 .and. (col > 2 .or. col < 7)) then
                if (board(row - 2, col - 2) == 0 .or. board(row - 2, col + 2) == 0) then
                    valid = .true.
                    return
                end if
            else
                valid = .true.
                return
            end if
        end if
    end if
end if
  end if

 ! If none of the conditions are met, the move is not valid
  valid = .false.

end function IsValidMove


		  


	function EvaluateMove(board, player, row, col) result(score)
  implicit none
  integer, intent(in) :: board(:,:)
  integer, intent(in) :: player
  integer, intent(in) :: row, col
  integer :: score

  ! Initialize score to 0
  score = 0

  ! Add points for capturing opponent's pieces
  if (player == 1) then
    if (row > 1) then
      if (col > 1 .and. board(row-1, col-1) == 2) then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (col < 8 .and. board(row-1, col+1) == 2) then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (row > 2) then
      if (col > 2 .and. board(row-1, col-1) == 2 .and. board(row-2, col-2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (col < 7 .and. board(row-1, col+1) == 2 .and. board(row-2, col+2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if

    if (row < 8) then
      if (col > 1 .and. board(row+1, col-1) == 2) then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (col < 8 .and. board(row+1, col+1) == 2) then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (row < 7) then
      if (col > 2 .and. board(row+1, col-1) == 1 .and. board(row+2, col-2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (col < 7 .and. board(row+1, col+1) == 1 .and. board(row+2, col+2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if
  

  ! Add points for moving closer to the opponent's side

    score = score + row  ! Add points based on how close the move is to row 8
  
 

  ! Add points for reaching the opponent's side and becoming a king
  if (row == 8) then
    score = score + 7  ! Add 7 points for reaching row 8 and becoming a king
   endif
  ! Add points for moving from the corner to be supported by the side of the board
  if (row == 1 .or. row == 8) then
score = score + 3  ! Add 3 points for moving from the corner
end if
    if (col == 1 .or. col == 8) then
      score = score + 3  ! Add 3 points for moving from the corner
	   end if
! Add 2 points for moving from the adjacent column to the corner
if (row == 2  .or.  row == 7 ) then 
score  =  score + 1
end if 
if (col == 2  .or.  col == 7) then
    score = score + 1
endif

! Add 1 point for each friendly pawn adjacent to the current pawn
    if (row > 1 .and. col > 1 .and. board(row-1, col-1) == 1) then
        score = score + 3
    endif
    if (row > 1 .and. col < 8 .and. board(row-1, col+1) == 1) then
        score = score + 3
    endif

    if (row < 8 .and. col > 1 .and. board(row+1, col-1) == 1) then
        score = score + 3
    endif
    if (row < 8 .and. col < 8 .and. board(row+1, col+1) == 1) then
        score = score + 3
    endif

 else if (player == 2) then
   
    if (row < 8) then
      if (col > 1 .and. board (row+1, col-1) == 1)  then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (col < 8 .and. board(row+1, col+1) == 1) then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (row < 7) then
      if (col > 2 .and. board(row+1, col-1) == 2 .and. board(row+2, col-2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (col < 7 .and. board(row+1, col+1) == 2 .and. board(row+2, col+2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if

    if (row > 1) then
      if (col > 1 .and. board(row-1, col-1) == 1) then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (col < 8 .and. board(row-1, col+1) == 1) then
        score = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (row > 2) then
      if (col > 2 .and. board(row-1, col-1) == 2 .and. board(row-2, col-2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (col < 7 .and. board(row-1, col+1) == 2 .and. board(row-2, col+2) == 0) then
        score = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if
  

  ! Add points for moving closer to the opponent's side
  score = score + (8 - row)  ! Add points based on how close the move is to row 1

  ! Add points for reaching the opponent's side and becoming a king
  if (row == 1) then
    score = score + 7  ! Add 7 points for reaching row 1 and becoming a king
  end if

  ! Add points for moving from the corner to be supported by the side of the board
  if (row == 1 .or. row == 8) then
    score = score + 3  ! Add 3 points for moving from the corner
  end if
  if (col == 1 .or. col == 8) then
    score = score + 3  ! Add 3 points for moving from the corner
  end if

  ! Add 2 points for moving from the adjacent column to the corner
  if (row == 2  .or.  row == 7 ) then 
    score  =  score + 1
  end if 
  if (col == 2  .or.  col == 7) then
    score = score + 1
  end if 

  ! Add 1 point for each friendly pawn adjacent to the current pawn
  if (row > 1 .and. col > 1 .and. board(row-1, col-1) == 2) then
    score = score + 3
  end if
  if (row > 1 .and. col < 8 .and. board(row-1, col+1) == 2) then
    score = score + 3
  end if
  if (row < 8 .and. col > 1 .and. board(row+1, col-1) == 2) then
    score = score + 3
  end if
  if (row < 8 .and. col < 8 .and. board(row+1, col+1) == 2) then
    score = score + 3
  end if
end if
  ! Return the final score
  EvaluateMove = score

  end function EvaluateMove

	  

  subroutine MakeMove(board, player, bestMoveRow, bestMoveCol)
  implicit none
  integer, dimension(8, 8) :: board
  integer :: player
  integer :: bestMoveRow, bestMoveCol

  ! Update the game board with the best move
  board(bestMoveRow, bestMoveCol) = player

end subroutine MakeMove


	 end subroutine player1
