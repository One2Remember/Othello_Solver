# some global variables, I know, I know
MOVE_OFFSETS = (-5, -4, -3, -1, 1, 3, 4, 5)
LIST_OF_ACTIONS = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
INF = 1000
PASS_TURN = -1

# checks if a given position is off the edge of the board given which direction we are checking
def is_not_off_edge(move, to_check, offset):
    if not 0 <= to_check < 16:
        return False
    if offset is -4 or offset is 4:   # for checking north, south
        return True
    if offset is -1:    # for westerly checks
        closest_edge = move - move % 4      # calculate edge of board
        return to_check >= closest_edge
    if offset is 1:     # for easterly checks
        closest_edge = move + 3 - move % 4  # calculate edge of board
        return to_check <= closest_edge
    if offset is -5 or offset is 3:    # for northwest, southwest
        return to_check is not move + offset * (move % 4 + 1)
    if offset is -3 or offset is 5:    # for northeast, southeast
        return to_check is not move + offset * (4 - move % 4)

def Result(state, action, color):
    if action is PASS_TURN:  # if action is pass
        return state  # board is unmodified
    opponent_color = 'B' if color is 'W' else 'W'  # determine opponents color
    new_state = '%s' % state  # make a local copy of state
    # for each cardinal direction, check if you can capture any opponents tiles,
    # if you can, set is_valid to True
    new_state = new_state[:action] + color + new_state[action + 1:]  # set that piece as moved
    # process all possible flips
    for offset in MOVE_OFFSETS:
        temp = action + offset
        # check if tile is opponent's
        if is_not_off_edge(action, temp, offset) and new_state[temp] is opponent_color:
            # move until off board or no longer opponent color
            while is_not_off_edge(action, temp, offset) and new_state[temp] is opponent_color:
                temp = temp + offset
            # if we have a flippable portion
            if is_not_off_edge(action, temp, offset) and new_state[temp] is color:
                # move back one space
                temp = temp - offset
                # flip until we get back to original position
                while temp is not action:
                    new_state = new_state[:temp] + color + new_state[temp + 1:]  # flip the tile
                    temp = temp - offset
    return new_state


# returns utility of a state given the color of the player as their pieces - opponent's pieces
def utility(state):
    black = 0
    white = 0
    for i in range(0, 16):
        if state[i] is 'B':
            black = black + 1
        elif state[i] is 'W':
            white = white + 1
    return black - white


# returns if move is valid
def valid_move(board, move, color):
    if board[move] is not '_':  # if board does not have blank space, it is invalid
        return False
    is_valid = False
    opponent_color = 'B' if color is 'W' else 'W'  # determine opponents color
    for offset in MOVE_OFFSETS:
        temp = move + offset
        # check if tile is opponent's
        if is_not_off_edge(move, temp, offset) and board[temp] is opponent_color:
            # move until off board or no longer opponent color
            while is_not_off_edge(move, temp, offset) and board[temp] is opponent_color:
                temp = temp + offset
            # if we have a flippable portion
            if is_not_off_edge(move, temp, offset) and board[temp] is color:
                is_valid = True
    return is_valid


def terminal_test(board):  # returns True if neither player can make a move
    move = 0
    while move < 16 and not valid_move(board, move, 'B') and not valid_move(board, move, 'W'):
        move = move + 1
    return True if move > 15 else False


def minimax_decision(state):
    best_utility = -INF  # loss
    best_action = PASS_TURN  # pass
    possible_action = 0
    while possible_action <= 15 and best_utility <= 0:
        if valid_move(state, possible_action, 'B'):  # if move is valid
            utility_of_new_state = min_value(Result(state, possible_action, 'B'))
            if utility_of_new_state > best_utility:  # no guaranteed win-con, but possible move
                best_utility = utility_of_new_state
                best_action = possible_action
        possible_action = possible_action + 1
    return best_action


def max_value(state):
    if terminal_test(state):
        return utility(state)
    this_utility = -INF
    for possible_action in LIST_OF_ACTIONS:
        if valid_move(state, possible_action, 'B'):   # if move is valid
            new_state = Result(state, possible_action, 'B')  # calc result of action
            this_utility = max(this_utility, min_value(new_state))
    return this_utility


def min_value(state):
    if terminal_test(state):
        return utility(state)
    this_utility = INF
    for possible_action in LIST_OF_ACTIONS:
        if valid_move(state, possible_action, 'W'):  # if move is valid
            new_state = Result(state, possible_action, 'W')  # calc result of action
            this_utility = min(this_utility, max_value(new_state))
    return this_utility


# prints a neatly formatted enumerated board state
def print_enum_board(board):
    for i in range(0, 16):
        print(str(i) + ": ", end="")
        if i < 10:
            print("", end=" ")
        print(str(board[i]), end="   ")
        if i % 4 is 3:
            print()

# prints a neatly formatted board state without enumeration
def print_board(board):
    for i in range(0, 16):
        print(str(board[i]), end="   ")
        if i % 4 is 3:
            print()

# prints board and asks player for their move (integer index of list)
def prompt_player_move(current_board):
    print("Where would you like to move? (type \"p\" to pass turn)")
    print_board(current_board)
    print()
    print_enum_board(current_board)
    print()
    move = input()
    if move is "p":  # let player pass
        return PASS_TURN
    move = int(move)  # convert to int
    while not valid_move(current_board, move, 'W'):
        print("That is not a valid move; try again")
        print_enum_board(current_board)
        move = input()
        if move is "p":  # let player pass
            return PASS_TURN
        move = int(move)  # convert to int
    return move


# player is white, computer is black
def main():
    board = "_____WB__BW_____"
    print("You play as White (W)")
    while not terminal_test(board):  # while the game has not ended yet
        cur_action = prompt_player_move(board)  # get player's move
        board = Result(board, cur_action, 'W')  # make player's move
        print_board(board)  # let player see board before computer makes decision
        comp_action = minimax_decision(board)  # get computer's move
        if comp_action is PASS_TURN:
            print("Computer passes")
        else:
            print("\nComputer chose to place piece at " + str(comp_action))
        board = Result(board, comp_action, 'B')  # make computer's move

    print("Final board: ")
    print_board(board)
    if utility(board) > 0:
        print("I win!")
    elif utility(board) < 0:
        print("You win!")
    else:
        print("Draw")

main()
