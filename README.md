Practical Assignment of Advanced Topics in Databases

Overview:
This project implements a Tetromino Puzzle Solver that demonstrates advanced database concepts and techniques. The system focuses on solving puzzles using the seven distinct one-sided tetrominoes (I, O, T, J, L, S, Z) through a combination of:

• Spatial Database Management: Using PostgreSQL with PostGIS extension to handle geometric shapes and spatial operations
• Deductive Database Integration: Coupling YAP Prolog with PostgreSQL for logical puzzle solving
• Advanced Database APIs: Implementing efficient database operations through C and Python interfaces
• Interactive Visualizations: Using Python and Matplotlib to display puzzles and solutions

The project implements a complete solution for:
1. Storing and managing tetromino pieces and puzzle boards using spatial data types
2. Solving puzzles through deductive reasoning using YAP-PostgreSQL integration
3. Visualizing both puzzles and their solutions in an interactive manner

Key Features:
• Spatial Operations: Translation, rotation, and difference operations for tetromino manipulation
• Puzzle Solving: Backtracking algorithm implementation in Prolog
• Interactive Visualization: Real-time display of puzzles and solutions
• Database Integration: Efficient storage and retrieval of spatial data

Part 1 - Database Structure:
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

Part 2 - YAP-PostgreSQL Integration:
The project includes a YAP-PostgreSQL integration that allows for deductive database operations. This integration is implemented through the following components:

1. yap2pgsql.pl:
   - Implements spatial predicates for geometric operations
   - Available predicates:
     * st_difference(Geom1, Geom2, Result): Calculates the difference between two geometries
     * st_translate(Geom, X, Y, Result): Moves a geometry by X,Y offset
     * st_rotate(Geom, Angle, Result): Rotates a geometry by specified angle

2. Database Connection:
   - Use db_open(Host, Port, DBName, User, Pass) to connect
   - Use db_close to disconnect
   - Use db_import(Query, Params, Result) for queries

Example YAP usage:
```prolog
?- db_open('localhost', 5432, 'postgres', 'user', 'pass').
?- st_translate('POLYGON((0 0, 2 0, 2 2, 0 2, 0 0))', 3, 4, Result).
?- db_close.
```

Part 3 - Visualization:
The project includes Python-based visualization tools for puzzles and solutions:

1. Available visualization scripts:
   - view_puzzles.py: Visualizes puzzle boards
   - view_solutions.py: Visualizes puzzle solutions
   - view_tetrominoes.py: Visualizes tetromino pieces

2. Features:
   - Interactive matplotlib visualizations
   - Color-coded tetrominoes
   - Grid-based display
   - Support for multiple puzzles

3. Running visualizations:
```bash
python view/view_puzzles.py  # View all puzzles
python view/view_solutions.py  # View solutions
python view/view_tetrominoes.py  # View tetromino pieces
```

Setup and Installation:
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
   git clone https://github.com/your-username/Advanced_Database.git
   cd Advanced_Database

2. Build the project:
   In the project directory, run the following command to compile the source code:
   make

3. Database Setup:
   Ensure your PostgreSQL database is set up correctly. You may need to import a sample database or configure the connection to your own instance.

Running the Project:
1. Compile the project:
   make

2. Run the main program:
   ./main

3. Run visualizations:
   python view/view_puzzles.py
   python view/view_solutions.py
   python view/view_tetrominoes.py

Configuration:
If you need to modify configuration settings (such as database credentials or table structures), update the configuration file config.json or modify the connection setup within the code.
