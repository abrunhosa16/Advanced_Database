# **Practical Assignment of Advanced Topics in Databases**

**Overview:**
This project implements a Tetromino Puzzle Solver that demonstrates advanced database concepts and techniques. The system focuses on solving puzzles using the seven distinct one-sided tetrominoes (I, O, T, J, L, S, Z) through a combination of:

• Spatial Database Management: Using PostgreSQL with PostGIS extension to handle geometric shapes and spatial operations
• Deductive Database Integration: Coupling YAP Prolog with PostgreSQL for logical puzzle solving
• Advanced Database APIs: Implementing efficient database operations through C and Python interfaces
• Interactive Visualizations: Using Python and Matplotlib to display puzzles and solutions

**Part 1 - Database Structure:**
The project implements a spatial database using PostgreSQL with PostGIS extension, consisting of three main tables:

1. Tetrominoes Table:
   - Stores the seven distinct one-sided tetrominoes (I, O, T, J, L, S, Z)
   - Attributes:
     * name: Identifier of the tetromino (I, O, T, J, L, S, Z)
     * color: RGB color code for visualization
     * type: Geometric type (Polygon)
     * geom: Spatial geometry using PostGIS

2. Puzzles Table:
   - Stores the puzzle boards to be solved
   - Attributes:
     * name: Unique identifier for each puzzle
     * color: Board color (typically black)
     * type: Geometric type (Polygon)
     * geom: Spatial geometry representing the puzzle board

3. Solutions Table:
   - Stores the solutions for each puzzle
   - Attributes:
     * puzzle_id: Reference to the puzzle
     * tetromino_id: Reference to the tetromino used
     * position: Spatial position of the tetromino
     * rotation: Rotation angle of the tetromino

**Part 2 - YAP-PostgreSQL Integration:**
The project includes a YAP-PostgreSQL integration that allows for deductive database operations. This integration is implemented through the following components:

1. yap2pgsql.pl:
   - Implements spatial predicates for geometric operations
   - Available predicates:
     * st_difference(Geom1, Geom2, Result): Calculates the difference between two geometries
     * st_translate(Geom, X, Y, Result): Moves a geometry by X,Y offset
     * st_rotate(Geom, Angle, Result): Rotates a geometry by specified angle
     * st_contains(Geom1, Geom2, Result): Checks if Geom1 contains Geom2

2. Database Connection:
   - Use db_open(Host, Port, DBName, User, Pass) to connect
   - Use db_close to disconnect
   - Use db_import(Query, Params, Result) for queries

**Part 3 - Python Visualization:**
The project includes Python-based visualization tools for puzzles and solutions:

1. view_backtracking_solutions.py:
   - Visualizes the backtracking solutions
   - Features:
     * Color-coded tetrominoes
     * Grid-based display
     * Piece labels
     * Board holes visualization
   - Output:
     * Saves solutions as PNG files in images/backtracking_solutions/
     * High-resolution images (300 DPI)
     * Clear piece placement visualization

**Part 4 - Tetris Solver Implementation:**
The project implements a backtracking solver in Prolog (tetris_solver.pl) with the following features:

1. Core Functions:
   - get_puzzle/2: Retrieves puzzle geometry
   - get_tetrominoes/1: Gets all available tetrominoes
   - try_place_piece/4: Tests piece placement
   - calculate_remaining_space/3: Computes available space
   - try_place_piece_with_rotation/5: Tests piece placement with rotation
   - try_all_rotations/4: Tests all possible rotations
   - solve_puzzle/1: Main solver predicate
   - solve_puzzle_recursive/4: Implements backtracking algorithm

2. Testing Functions:
   - test_db/0: Tests database connection
   - test_geometries/0: Tests puzzle and tetromino geometries
   - test_yap_translate/0: Tests translation operations
   - test_yap_rotate/0: Tests rotation operations
   - test_yap_difference/0: Tests difference operations
   - test_yap_fit/0: Tests piece fitting
   - test_all_positions/0: Tests all possible positions
   - test_all_rotations/0: Tests all possible rotations
   - test_solver/0: Tests the complete solver

**Setup and Installation:**
Prerequisites:
Before you begin, ensure you have the following installed:
- Make (for building the project)
- GCC (or another C++ compiler)
- PostgreSQL (for database functionality)
- PostGIS for spatial data
- YAP Prolog (for deductive database functionality)
- Python 3.x (for visualization)
- Python packages: psycopg2, matplotlib, shapely

Installation Steps:
1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/Advanced_Database.git
   cd Advanced_Database
   ```

2. Build the project:
   In the project directory, run the following commands to compile the source code:
   ```bash
   make clean    # Clean previous builds
   make         # Build the project
   make install # Install the project
   ```

3. Database Setup:
   Ensure your PostgreSQL database is set up correctly. You may need to import a sample database or configure the connection to your own instance.

4. Python Environment Setup:
   ```bash
   python -m venv enviroment
   source enviroment/bin/activate
   pip install psycopg2-binary shapely matplotlib
   ```

**Running the Project:**
1. Compile the project:
   ```bash
   make clean    # Clean previous builds
   make         # Build the project
   ```

2. Run the solver:
   ```prolog
   ?- [yap2pgsql].                    # Load YAP-PostgreSQL interface
   ?- [tetris_solver].                # Load Tetris solver
   ?- db_open('localhost', 5432, 'postgres', 'tvmarcon', '1234').  # Connect to database
   ?- test_solver.                    # Run the solver
   ```

3. Run visualizations:
   ```bash
   python view/view_backtracking_solutions.py
   ```

**Configuration:**
If you need to modify configuration settings (such as database credentials or table structures), update the configuration file config.json or modify the connection setup within the code.
