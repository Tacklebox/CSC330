# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here:

  # Your Enhancements here
  def initialize (point_array, board)
    super
  end

  def self.next_piece (board)
    MyPiece.new(All_Pieces.sample, board)
  end

  All_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]], # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[-2, 0], [0, 0], [-1, 0], [1, 0], [2, 0]], # extra long (only needs two)
               [[0, -2], [0, 0], [0, -1], [0, 1], [0, 2]]],
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [1, 0], [1, 1]]), # filled in L
               rotations([[0, 0], [1, 0], [0, 1]]), # little L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z
end

class MyBoard < Board
  # Your Enhancements here:
  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat_next = false
  end

  def cheat
    if @cheat_next == false and @score >= 100 then
      @cheat_next = true
      @score -= 100
    end
  end

  def next_piece
    if @cheat_next then
      @current_block = MyPiece.new([[[0, 0]]], self)
      @cheat_next = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # Your Enhancements here:
  def initialize
    super
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    super
    @root.bind('u', proc {
      @board.rotate_clockwise
      @board.rotate_clockwise
    })

    @root.bind('c', proc {@board.cheat})
  end
end


