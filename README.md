Subroutine player1 (Board,Move,Player)
Implicit None
Integer, intent(in) :: board(8,8), Player
Integer, intent(out) :: Move(2,2)

! Variables for tracking the best move
Integer :: bestMoveRow, bestMoveCol
Integer :: bestMoveScore
Logical :: foundBestMove
Integer :: row, col
Integer :: i,j
logical :: valid
integer :: score
integer :: k, p
integer, dimension(8, 8) :: scoree		
			integer, dimension(64) :: valid_rows, nut_rows
integer, dimension(64) :: valid_cols, nut_cols
integer :: valid_count, nut_count
			    bestMoveScore = -1
  foundBestMove = .false.

valid_count = 0
nut_count = 0

! Internal subroutines

! Initialize variables
bestMoveScore = -1
foundBestMove = .false. 

			  if (player == 1) then
                  row = 0
                  col = 0
	   do row = 1, 8
  do col = 1, 8
    ! Check if the current position is a valid move for the player

    ! Check if the piece is equal to 1
    if (board(row, col) == 1 .or. board(row, col) == 11) then
      nut_count = nut_count + 1
      nut_rows(nut_count) = row
      nut_cols(nut_count) = col
    end if
  end do
  end do


else if (player ==2) then
row = 0
col = 0
   do row = 1, 8
  do col = 1, 8
    ! Check if the current position is a valid move for the player

    ! Check if the piece is equal to 1
    if (board(row, col) == 2 .or. board(row, col) == 22) then
      nut_count = nut_count + 1
      nut_rows(nut_count) = row
      nut_cols(nut_count) = col
    end if
  end do
end do
end if

row = 0
col = 0
! Iterate over each position on the board
do row = 1, 8
  do col = 1, 8
    ! Check if the current position is a valid move for the player

  
  ! Check if the player is moving a normal checker or a king checker
   if (player == 1) then																				!if_1
    ! Check if the piece is a normal checker
     if (board(row, col) == 1) then																		 !if_2
        ! Check if the player can move diagonally down
         if (row < 8 .and. (col > 1 .or. col < 8)) then													  !if_3

            ! Check if the position below and to the left/right is occupied by the opponent's piece
            ! Check if the position below and to the left/right is empty
             if (board(row + 1, col - 1) == 0) then																!if_4
                valid = .true.
				  valid_count = valid_count + 1
            valid_rows(valid_count) = row+1
            valid_cols(valid_count) = col-1
			end if																							  !end if_4
			 if (board(row + 1, col + 1) == 0) then																	!if_5
                 
			valid = .true.
			valid_count = valid_count + 1
            valid_rows(valid_count) = row+1
            valid_cols(valid_count) = col+1
			  end if																						!end if_5
			  end if																						 !end if_3
			  
			  
			  
			  
			       if (row < 7 .and. (col > 2 .or. col < 7)) then      !if_6   
				                                													 
            if (board(row + 1, col - 1) == 2) then 						!if_7   
                
                			
                    if (board(row + 2, col - 2) == 0) then				!if_8	 
                        valid = .true.
						valid_count = valid_count + 1
            valid_rows(valid_count) = row+2
            valid_cols(valid_count) = col-2
					  end if											!end if_8
					  end if												!end if_7
					  
					  
					  
					  												  ! if_9
					  if (board(row + 1, col + 1) == 2) then													 
                 if (board(row + 2, col + 2) == 0) then				 !if_10
                        valid = .true.
						valid_count = valid_count + 1
            valid_rows(valid_count) = row+2
            valid_cols(valid_count) = col+2   
                    end if 										 !end if 10
                	 end if !									  ! end if 9
                end if 										  !end if 6
            	
    		

    else if (board(row, col) == 11) then						  !hamon if 2 k bara mohre bood
        ! Check if the player can move diagonally down
         if (row < 8 .and. (col > 1 .or. col < 8)) then				  !if 11

            ! Check if the position below and to the left/right is occupied by the opponent's piece
            ! Check if the position below and to the left/right is empty

            if (board(row + 1, col - 1) == 0) then                       !if 12
                valid = .true.
				valid_count = valid_count + 1
            valid_rows(valid_count) = row+1
            valid_cols(valid_count) = col-1
					   end if											!end if 12


				if (board(row + 1, col + 1) == 0) then 				   !if 13
										  valid = .true.
				valid_count = valid_count + 1
            valid_rows(valid_count) = row+1
            valid_cols(valid_count) = col+1							  !end if 13
					   end if
						end if                                        !end if 11

					   if (row < 7 .and. (col > 2 .or. col < 7)) then      !if 14

             if (board(row + 1, col - 1) == 2) then  !if 15
                ! Check if the back of the opponent's piece is open
                
                    if (board(row + 2, col - 2) == 0) then !if 16
                        valid = .true.
						valid_count = valid_count + 1
            valid_rows(valid_count) = row+2
            valid_cols(valid_count) = col-2

                    end if 								   !end if 16
                else										!end if 15
                    														  !if 17
					 if (board(row + 1, col + 1) == 2) then					   ! if 18 
								  if (board(row + 2, col + 2) == 0) then
								  
								  valid = .true.
						valid_count = valid_count + 1
            valid_rows(valid_count) = row+2
            valid_cols(valid_count) = col+2
								    
                end if 															!end if 18

            end if	 															  ! end if 17
        
			 end if	  																 !end if 14
			  																			!if 1 v 2 bazn hanoo
			
        ! Check if the player can move diagonally up
        if (row > 1 .and. (col > 1 .or. col < 8)) then									!if 19
            ! Check if the position above and to the left/right is occupied by the opponent's piece
            ! Check if the position above and to the left/right is empty
            if (board(row - 1, col - 1) == 0) then	  !if 20
                valid = .true.
				valid_count = valid_count + 1
            valid_rows(valid_count) = row-1
            valid_cols(valid_count) = col-1
					   end if						   !end if 20
					   
					   	   if (board(row - 1, col + 1) == 0) then	 !if 21
					   					   valid = .true.
				valid_count = valid_count + 1
            valid_rows(valid_count) = row-1
            valid_cols(valid_count) = col+1
						  end if 							   !wnd if 21
						  End if							 !end if 19
					   
					   
					   
				if (row > 2 .and. (col > 2 .or. col < 7)) then		 !if 22

             if (board(row - 1, col - 1) == 2) then					   	!if 23
                ! Check if the back of the opponent's piece is open
              		 
                     if (board(row - 2, col - 2) == 0) then					! if 24
                        valid = .true.
						valid_count = valid_count + 1
            valid_rows(valid_count) = row-2
            valid_cols(valid_count) = col-2

                    end if 													   !end if 24
                end if																!end if 23
																			  
						if (board(row - 1, col + 1) == 2) then						  !if 25
							if (board(row - 2, col + 2) == 0) then					! if 26
											valid = .true.
						valid_count = valid_count + 1
            valid_rows(valid_count) = row-2
            valid_cols(valid_count) = col+2
							end if 					   ! end if 26
							end if 						! end if 25
							end if						! end if 22						
                    

                    
                
             
        end if 	 !end if 2
			  		   
		   end if	   ! end if 1

       		else if (player == 2) then                                        !if_1
    ! Check if the piece is a normal checker
     if (board(row, col) == 2) then                                     !if_2
        ! Check if the player can move diagonally down
         if  (1< row  .and. (col > 1 .or. col < 8)) then                            !if_3

            ! Check if the position below and to the left/right is occupied by the opponent's piece
            ! Check if the position below and to the left/right is empty
             if (board(row -1, col - 1) == 0) then                                !if_4
                valid = .true.
          valid_count = valid_count + 1
            valid_rows(valid_count) = row-1
            valid_cols(valid_count) = col-1
      end if                                                !end if_4
       if (board(row - 1, col + 1) == 0) then                                  !if_5
                 
      valid = .true.
      valid_count = valid_count + 1
            valid_rows(valid_count) = row-1
            valid_cols(valid_count) = col+1
        end if                                            !end if_5
        end if                                             !end if_3
        
        
        
        
             if (2< row .and. (col > 2 .or. col < 7)) then      !if_6   
                                                                   
            if (board(row - 1, col - 1) == 1) then             !if_7   
                
                      
                    if (board(row - 2, col - 2) == 0) then        !if_8   
                        valid = .true.
            valid_count = valid_count + 1
            valid_rows(valid_count) = row-2
            valid_cols(valid_count) = col-2
            end if                      !end if_8
            end if                        !end if_7
            
            
            
                                      ! if_9
            if (board(row - 1, col + 1) == 1) then                           
                 if (board(row - 2, col + 2) == 0) then         !if_10
                        valid = .true.
            valid_count = valid_count + 1
            valid_rows(valid_count) = row-2
            valid_cols(valid_count) = col+2   
                    end if                      !end if 10
                   end if !                    ! end if 9
                end if                       !end if 6
              
        

    else if (board(row, col) == 22)  then              !hamon if 2 k bara mohre bood
        ! Check if the player can move diagonally down
         if (1< row .and. (col > 1 .or. col < 8)) then          !if 11

            ! Check if the position below and to the left/right is occupied by the opponent's piece
            ! Check if the position below and to the left/right is empty

            if (board(row - 1, col - 1) == 0) then                       !if 12
                valid = .true.
        valid_count = valid_count + 1
            valid_rows(valid_count) = row-1
            valid_cols(valid_count) = col-1
             end if                      !end if 12


        if (board(row -1, col + 1) == 0) then            !if 13
                      valid = .true.
        valid_count = valid_count + 1
            valid_rows(valid_count) = row-1
            valid_cols(valid_count) = col+1                !end if 13
             end if
            end if                                        !end if 11

             if (2< row .and. (col > 2 .or. col < 7)) then      !if 14

			  if (board(row - 1, col - 1) == 1) then  !if 15
                ! Check if the back of the opponent's piece is open
                
                    if (board(row - 2, col - 2) == 0) then !if 16
                        valid = .true.
            valid_count = valid_count + 1
            valid_rows(valid_count) = row-2
            valid_cols(valid_count) = col-2

                    end if                    !end if 16
                else                    !end if 15
                                                  !if 17
           if (board(row - 1, col + 1) == 1) then             ! if 18 
                  if (board(row - 2, col + 2) == 0) then
                  
                  valid = .true.
            valid_count = valid_count + 1
            valid_rows(valid_count) = row-2
            valid_cols(valid_count) = col+2
                    
                end if                               !end if 18

            end if                                   ! end if 17
        
       end if                                     !end if 14
                                              !if 1 v 2 bazn hanoo
      
        ! Check if the player can move diagonally up
        if (8 > row .and. (col > 1 .or. col < 8)) then                  !if 19
            ! Check if the position above and to the left/right is occupied by the opponent's piece
            ! Check if the position above and to the left/right is empty
            if (board(row +1, col - 1) == 0) then    !if 20
                valid = .true.
        valid_count = valid_count + 1
            valid_rows(valid_count) = row+1
            valid_cols(valid_count) = col-1
             end if               !end if 20
             
                  if (board(row + 1, col + 1) == 0) then   !if 21
                          valid = .true.
        valid_count = valid_count + 1
            valid_rows(valid_count) = row+1
            valid_cols(valid_count) = col+1
              end if                  !wnd if 21
              End if               !end if 19
             
             
             
        if (7> row .and. (col > 2 .or. col < 7)) then     !if 22

             if (board(row + 1, col - 1) == 1) then               !if 23
                ! Check if the back of the opponent's piece is open
                   
                     if (board(row + 2, col - 2) == 0) then          ! if 24
                        valid = .true.
            valid_count = valid_count + 1
            valid_rows(valid_count) = row+2
            valid_cols(valid_count) = col-2

                    end if                              !end if 24
                end if                                !end if 23
                                        
            if (board(row +1, col + 1) == 1) then              !if 25
              if (board(row +2, col + 2) == 0) then          ! if 26
                      valid = .true.
            valid_count = valid_count + 1
            valid_rows(valid_count) = row+2
            valid_cols(valid_count) = col+2
              end if              ! end if 26
              end if             ! end if 25
              end if            ! end if 22            
                    

                    
                
             
        end if    !end if 2
               
       end if     ! end if 1
    
  end if

 ! If none of the conditions are met, the move is not valid
  valid = .false.

		end do
end do

	  

	k=0	  
  ! Initialize score to 0
  score = 0
		    do k = 1, size(valid_rows)


    i = valid_rows(k)
    j = valid_cols(k)
	
  ! Add points for capturing opponent's pieces
  if (player == 1) then
    if (i > 1) then
      if (col > 1 .and. board(i-1, j-1) == 2) then
         scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (j < 8 .and. board(i-1, j+1) == 2) then
         scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (i > 2) then
      if (j > 2 .and. board(i-1, j-1) == 2 .and. board(i-2, j-2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (j < 7 .and. board(i-1, j+1) == 2 .and. board(i-2, j+2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if

    if (i < 8) then
      if (j > 1 .and. board(i+1, j-1) == 2) then
         scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (j < 8 .and. board(i+1, j+1) == 2) then
         scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (i < 7) then
      if (j > 2 .and. board(i+1, j-1) == 1 .and. board(i+2, j-2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (j < 7 .and. board(i+1, j+1) == 1 .and. board(i+2, j+2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if
  

  ! Add points for moving closer to the opponent's side

     scoree(i, j) = score + j  ! Add points based on how close the move is to row 8
  
 

  ! Add points for reaching the opponent's side and becoming a king
  if (i == 8) then
    scoree(i, j) = score + 7  ! Add 7 points for reaching row 8 and becoming a king
   endif
  ! Add points for moving from the corner to be supported by the side of the board
  if (i == 1 .or. i == 8) then
 scoree(i, j) = score + 3  ! Add 3 points for moving from the corner
end if
    if (j == 1 .or. j == 8) then
       scoree(i, j) = score + 3  ! Add 3 points for moving from the corner
	   end if
! Add 2 points for moving from the adjacent column to the corner
if (i == 2  .or.  i == 7 ) then 
 scoree(i, j)  =  score + 1
end if 
if (j == 2  .or.  j == 7) then
     scoree(i, j) = score + 1
endif

! Add 1 point for each friendly pawn adjacent to the current pawn
    if (i > 1 .and. j > 1 .and. board(i-1, j-1) == 1) then
         scoree(i, j) = score + 3
    endif
    if (i > 1 .and. j < 8 .and. board(i-1, j+1) == 1) then
         scoree(i, j) = score + 3
    endif

    if (i < 8 .and. j > 1 .and. board(i+1, j-1) == 1) then
         scoree(i, j) = score + 3
    endif
    if (i < 8 .and. j < 8 .and. board(i+1, j+1) == 1) then
         scoree(i, j) = score + 3
    endif
				  ! Update the best move if the current move has a higher score
    if ( scoree(i, j) > bestMoveScore) then
      bestMoveRow = i
      bestMoveCol = j
        bestMoveScore = scoree(row, col)
      foundBestMove = .true.
      
		end if 


 else if (player == 2 .and. board(i,j)==(0)) then
   
    if (i < 8) then
      if (j > 1 .and. board (i+1, j-1) == 1)  then
        scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (j < 8 .and. board(i+1, j+1) == 1) then
         scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (i < 7) then
      if (j > 2 .and. board(i+1, j-1) == 2 .and. board(i+2, j-2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (j < 7 .and. board(i+1, j+1) == 2 .and. board(i+2, j+2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if

    if (i > 1) then
      if (j > 1 .and. board(i-1, j-1) == 1) then
         scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
      if (j < 8 .and. board(i-1, j+1) == 1) then
         scoree(i, j) = score + 2  ! Add 1 point for capturing opponent's normal piece
      end if
    end if
    if (i > 2) then
      if (j > 2 .and. board(i-1, j-1) == 2 .and. board(i-2, j-2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
      if (j < 7 .and. board(i-1, j+1) == 2 .and. board(i-2, j+2) == 0) then
         scoree(i, j) = score + 19  ! Add 2 points for capturing opponent's piece and opening up a space
      end if
    end if
  

  ! Add points for moving closer to the opponent's side
   scoree(i, j) = score + (8 - i)  ! Add points based on how close the move is to row 1

  ! Add points for reaching the opponent's side and becoming a king
  if (i == 1) then
     scoree(i, j) = score + 7  ! Add 7 points for reaching row 1 and becoming a king
  end if

  ! Add points for moving from the corner to be supported by the side of the board
  if (i == 1 .or. i == 8) then
     scoree(i, j) = score + 3  ! Add 3 points for moving from the corner
  end if
  if (j == 1 .or. j == 8) then
    scoree(i, j) = score + 3  ! Add 3 points for moving from the corner
  end if

  ! Add 2 points for moving from the adjacent column to the corner
  if (i == 2  .or.  i == 7 ) then 
     scoree(i, j)  =  score + 1
  end if 
  if (j == 2  .or.  j == 7) then
    scoree(i, j) = score + 1
  end if 

  ! Add 1 point for each friendly pawn adjacent to the current pawn
  if (i > 1 .and. j > 1 .and. board(i-1, j-1) == 2) then
     scoree(i, j) = score + 3
  end if
  if (i > 1 .and. j < 8 .and. board(i-1, j+1) == 2) then
    scoree(i, j) = score + 3
  end if
  if (i < 8 .and. j > 1 .and. board(i+1, j-1) == 2) then
     scoree(i, j) = score + 3
  end if
  if (i < 8 .and. j < 8 .and. board(i+1, j+1) == 2) then
    scoree = score + 3
  end if
   scoree(i, j) = score  ! Store the score for the current position

end if
  ! Return the final score
  			


	
			  ! Update the best move if the current move has a higher score
    if ( scoree(i, j) > bestMoveScore) then
      bestMoveRow = i
      bestMoveCol = j
        bestMoveScore = scoree(row, col)
      foundBestMove = .true.
      
	end if 
	
	  
  
end do 

		 

! Make the best move if a valid move was found
IF (foundBestMove) THEN
  DO p = 1, nut_count
    row = nut_rows(p)
    col = nut_cols(p)


	!aval ezafi
	    IF (((bestMoveRow == row-1 .AND. bestMoveCol == col-1) .OR. &
        (bestMoveRow == row+1 .AND. bestMoveCol == col-1) .OR. &
        (bestMoveRow == row+1 .AND. bestMoveCol == col+1) .OR. &
        (bestMoveRow == row-1 .AND. bestMoveCol == col+1) .OR. &
        (bestMoveRow == row-2 .AND. bestMoveCol == col-2) .OR. &
        (bestMoveRow == row+2 .AND. bestMoveCol == col-2) .OR. &
        (bestMoveRow == row+2 .AND. bestMoveCol == col+2) .OR. &
        (bestMoveRow == row-2 .AND. bestMoveCol == col+2)) .eqv. .true.) THEN
		return

      Move(1, 1) = row
      Move(1, 2) = col
      Move(2, 1) = bestMoveRow
      Move(2, 2) = bestMovecol
    END if
   		!akhar ezafi

	
  END DO
END IF


 	
	
End Subroutine player1
