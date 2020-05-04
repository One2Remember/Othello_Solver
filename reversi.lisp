;
; reversi3.lisp (reversi v3)
;
; Emmanuel Gallegos
; 4/1/2020
; CS 461
; Reversi
;
; Runs a 4x4 game of Reversi using the common rule set of Othello, which begins with the
; center four squares of the board occupied with 2 each of white's and black's pieces.
; Passing is only legal if the active player has no other available moves.
; CPU is Black, Player is White. Player goes first.
;
; The minimax-decision algorithm used below is adapted from "Artificial Intelligence: 
; A Modern Approach, 3rd ed" by Russel and Norvig. The following functions from the text
; are directly implemented below:
;
; utility(state) - returns utility of state as black's advantage over white 
; terminal-test(state) - returns t if neither player has any valid moves left (besides passing)
; result(state, move, color)  - returns new 'state' with color 'player' making move 'move'
; minimax-decision(board) - adapted version of book's minimax to get CPU's move
; max-value(state) - adapted version of book's max-value for determining CPU's best move
; min-value(state) - adapted version of book's min-value for determining Player's best move
;
; The action() function is not implemented, as the possible actions are simply the board positions
; 0 through 15, and it is simple enough to check if an action is legal using a single line of
; code to call my function is-valid(), rendering a separate function call unecessary.
;
; Other helper methods are implemented as well as needed. Hopefully their use is obvious given
; their signatures and the comment above each one respectively
;
; It may be worth nothing that 'board' and 'state' are not used interchangeably. The 'board' refers
; to the actual current board state, while a 'state' is any theoretical board state including
; the current board state. Whenver possible, the most appropriate word is used in each function,
; as some functions are entirely used for one or the other, with some exceptions, such as result(),
; which is used both in computing theoretical states while searching and when making actual moves
; on the board.
;

; prints a neatly formatted board representing the current game state
(defun print-board(board)
	(terpri)
	(loop for i from 0 to 15
		do  (format t " ~a  " (elt board i))
			(when (= (mod i 4) 3) (terpri))
	)
	(terpri)
)

; prints enumerated board positions to help player pick their move
(defun print-nums()
	(write-string " 0   1   2   3")
	(terpri)
	(write-string " 4   5   6   7")
	(terpri)
	(write-string " 8   9   10  11")
	(terpri)
	(write-string " 12  13  14  15")
	(terpri)
)
; returns the utility of the board state as black's current advantage over white
(defun utility(state)
	(setf black 0)
	(setf white 0)
	; iterate over state
	(loop for i from 0 to 15 do
		; count if piece is black or white
		(if (equal (elt state i) 'W) 
			(setf white (+ white 1)) 
		)
		(if (equal (elt state i) 'B) 
			(setf black (+ black 1)) 
		)
	)
	; return black's advantage
	(- black white)
)

; a helper function that checks if a given position is still on the board for
; the purpose of capturing opponent's pieces
(defun is-not-off-edge(move to-check offset)
	; assume to-check is off edge off board
	(setf check nil)
	; ensure to-check is in range
	(if (and (>= to-check 0)(< to-check 16))
		(cond
			; for checking north, south
			((or(= offset -4)(= offset 4)) 
				(setf check t)
			)
			; for westerly checks
			((= offset -1) 
				(setf check (>= to-check (- move (mod move 4))))
			)
			; for easterly checks
			((= offset 1)
				(setf check (<= to-check (-(+ 3 move)(mod move 4))))
			)
			; for southwesterly and northwesterly checks
			((or(= offset 3)(= offset -5))
				(setf check (not(equal to-check (*(+ move offset)(+ 1 (mod move 4))))))
			)
			; for northeasterly and southeasterly checks
			((or(= offset -3)(= offset 5))
				(setf check (not(equal to-check (*(+ move offset)(- 4 (mod move 4))))))
			)
		)
	)
	check	; return whether position is on the board
)
; returns whether a given move by player: color is valid given a theoretical board state
(defun is-valid(state move color)
	(setf valid nil)	; assume not valid		
	; if index is not out of bounds and space is empty
	(if (and (and (>= move 0)(<= move 15)) (equal (elt state move) '_ ))
		(progn
			; determine opponent's color 
			(if (equal color 'B) (setf opponent-color 'W) (setf opponent-color 'B))
			; loop over all 8 cardinal directions
			(loop for offset in OFFSETS do 
				; get temp tile index to examine
				(setf temp (+ move offset))
				; if that tile is not off edge of board and it is the opponent's color
				(if (and (is-not-off-edge move temp offset) (equal (elt state temp) opponent-color))
					(progn
						; keep checking next tile until fallen off board or no longer the opponent's color
						(loop while (and (is-not-off-edge move temp offset) (equal (elt state temp) opponent-color)) do	
							(setf temp (+ temp offset))
						)
						; if we still haven't fallen off the edge and we've encountered another of our own color
						(if (and (is-not-off-edge move temp offset) (equal (elt state temp) color))
							; our board is valid
							(setf valid t)
						)
					)
				)
			)	
		)
	)
	valid	; return validity of move
)
; returns if a player has any valid moves left (besides passing)
(defun valid-moves-left(state color)
	(setf valid-moves t)	; assumes player has valid moves
	(setf move 0)
	; while move < 16 and player 'color' cannot make that move
	(loop while (and(< move 16)(not(is-valid state move color))) do
		(setf move (+ move 1))	; move++
	)
	(if (> move 15) (setf valid-moves nil))	; if move > 15, player has no moves available
	valid-moves
)

; tests if neither player has any available moves (pass does not count as a valid move)
(defun terminal-test(state)
	(not(or(valid-moves-left state 'B)(valid-moves-left state 'W)))
)
; returns computers move given the current board
(defun minimax-decision(board)	
	(setf my-move PASS_TURN)	; assume best move is to pass turn
	(setf highest-util NEG_INF)	; assume best move has utility -inf
	(setf potential-move 0)	; initialize first potential move to check
	; loop over all possible actions or until we find a winning move
	(loop while (and(<= potential-move 15)(<= highest-util 0)) do
		(if (is-valid board potential-move 'B)	; if said action IS actually possible
			(progn
				(setf next-utility (min-value (result board potential-move 'B)))
				; check if its a better move than our current best
				(if (> next-utility highest-util)
					(progn
						(setf my-move potential-move)	; set my move to this winning move
						(setf highest-util next-utility)	; set my utility to positive
					)
				)
			)
		)
		(setf potential-move (+ potential-move 1))	; potential-move++
	)
	my-move
)

; returns utility of computers best move given board state and the assumption the player will play optimally
(defun max-value(state)
	(if (terminal-test state) 
		(setf max-val (utility state))	; set return value to utility of state
	; else
		(progn
			(setf max-val NEG_INF)	; v <-- -inf
			; for each a in actions
			(loop for potential-move from 0 to 15 do
				(if (is-valid state potential-move 'B)	; if said action IS actually possible
					(setf max-val (max max-val (min-value (result state potential-move 'B))))
				)
			)
		)
	)
	max-val
)

; returns utility of player's best move given board state and the assumption the computer will play optimally
(defun min-value(state)
	(if (terminal-test state) 
		(setf min-val (utility state))	; set return value to utility of state
	; else
		(progn
			(setf min-val POS_INF)	; v <-- inf
			; for each a in actions
			(loop for potential-move from 0 to 15 do
				(if (is-valid state potential-move 'W)	; if said action IS actually possible
					(setf min-val (min min-val (max-value (result state potential-move 'W))))
				)
			)
		)
	)
	min-val
)


; returns the result of the 'move' being made on the 'board' by player: 'color'
(defun result(state move color)
	(setf return-state (copy-list state))	; make a copy of the state to return
	(if (/= move PASS_TURN)
		(progn
			; determine opponent's color 
			(if (equal color 'B) (setf opponent-color 'W) (setf opponent-color 'B))
			(setf (elt return-state move) color)	; flip the particular tile
			(loop for offset in OFFSETS do 	; loop over all 8 cardinal directions
				(setf temp (+ move offset))	; get temp tile index to examine
				; if that tile is not off edge of state and it is the opponent's color
				(if (and (is-not-off-edge move temp offset) (equal (elt return-state temp) opponent-color))
					(progn
						; keep checking next tile until fallen off state or no longer the opponent's color
						(loop while (and (is-not-off-edge move temp offset) (equal (elt return-state temp) opponent-color)) do	
							(setf temp (+ temp offset))
						)
						; if we still haven't fallen off the edge and we've encountered another of our own color
						(if (and (is-not-off-edge move temp offset) (equal (elt return-state temp) color))
							(progn	; make the capture
								(setf temp (- temp offset))	; move back one space
								(loop while (/= temp move) do	; until we reach our starting position
									(setf (elt return-state temp) color)	; flip the tile
									(setf temp (- temp offset))		; scoot back one space
								)
							)
						)
					)
				)
			)	
		)
	)
	return-state	; return new board state
)

; gets and validates the player's move as a number in [-1,15], with -1 being pass
(defun prompt-player-move(board)
	(terpri)
	(write-string "Where would you like to move? (type 0-15 or -1 to pass turn)")
	(terpri)
	(print-board board)
	(terpri)
	(print-nums)
	(terpri)
	(setf player-choice (read))	; read player choice
	; check if player is trying to pass when they shouldn't be
	(setf invalid_pass nil)
	(if (and(= player-choice PASS_TURN)(valid-moves-left board 'W))
		(setf invalid_pass t)
	)
	; force player to pick valid move if their move is invalid
	(loop while (or invalid_pass (not (is-valid board player-choice 'W))) do
		(write-string "That is not a valid move, try again")
		(terpri)
		(print-board board)
		(setf player-choice (read))
		(setf invalid_pass nil)
		; check if player tried to pass illegally again
		(if (and(= player-choice PASS_TURN)(valid-moves-left board 'W))
			(setf invalid_pass t)
		)
	)
	player-choice
)

; runs the game in console window
(defun reversi()
	; some useful constants
	(defconstant PASS_TURN -1)	; represents the player's (or CPU's) choice to pass the turn
	(defconstant NEG_INF -10000)  ; represents -inf in min-value
	(defconstant POS_INF 10000)	; represents +inf in min-value
	(defconstant OFFSETS (list -5 -4 -3 -1 1 3 4 5))  ; for checking each of 8 directions
	(defvar board (list '_ '_ '_ '_ '_ 'W 'B '_ '_ 'B 'W '_ '_ '_ '_ '_))	; the game board
	(terpri)
	(write-string "Welcome to Reversi. You play as White (W)")
	; main loop - gets player move and responds with comp move until game over
	(loop while (not (terminal-test board)) do
		(terpri)
		(setf player-choice (prompt-player-move board))	; get player's move
		(setf board (result board player-choice 'W))	; make player's move
		(print-board board)							; print the board
		(setf comp-choice (minimax-decision board))	; get CPU's move	
		(format t "Computer chose: ~a" comp-choice)	; print what CPU chose to do
		(setf board (result board comp-choice 'B))	; make CPU's move	
	)
	(terpri)
	(print-board board)
	(if(> (utility board) 0)
		(write-string "CPU Wins")
	; else
		(write-string "Player Wins")
	)
)

(reversi)