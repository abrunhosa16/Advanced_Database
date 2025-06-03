import psycopg2
from shapely import wkb, wkt
from shapely.affinity import rotate, translate
import matplotlib.pyplot as plt
import json
import re
import os

with open('config.json') as f:
    config = json.load(f)

conn = psycopg2.connect(**config)

def parse_tetris_output(input_str):
    """Parse tetris output to extract polygon and placements"""
    # Extract polygon
    polygon_match = re.search(r'POLYGON\(\([^\)]+\)(?:,\([^\)]+\))*\)', input_str)
    polygon = polygon_match.group(0) if polygon_match else None
    
    # Extract pieces
    piece_pattern = re.compile(r'placed\(([A-Z]),\((\d+),(\d+)\),(\d+)\)')
    placements = []
    for match in piece_pattern.finditer(input_str):
        name = match.group(1)
        x = int(match.group(2))
        y = int(match.group(3))
        rotation = match.group(4)
        placements.append((name, rotation, x, y))
    
    return polygon, placements

def get_tetrominoe_data(tetro_name):
    """Fetch tetrominoe data from database"""
    cur = conn.cursor()
    cur.execute('''
        SELECT name, color, geom
        FROM tetrominoes
        WHERE name = %s;
    ''', (tetro_name,))
    row = cur.fetchone()
    
    if row:
        name, color, geom_wkb = row
        tetro = wkb.loads(geom_wkb)
        
        # Parse color
        r, g, b, a = map(int, color.split(','))
        rgba_color = (r / 255, g / 255, b / 255, a / 255)
        
        return tetro, rgba_color
    return None, None

def transform_piece(polygon, rotation, x, y):
    """Apply rotation and translation to a piece"""
    rotated = rotate(polygon, rotation, origin=(0,0), use_radians=False)
    translated = translate(rotated, xoff=x, yoff=y)
    return translated

def plot_tetris_solution_from_db(polygon_wkt, placements, title="Backtracking Solution"):
    """Plot tetris solution using database pieces"""
    
    # Parse base polygon
    base_polygon = wkt.loads(polygon_wkt)
    
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot base polygon exterior
    x, y = base_polygon.exterior.xy
    ax.fill(x, y, alpha=0.3, edgecolor='black', facecolor='lightgray', linewidth=3)
    
    # Plot base polygon holes
    for hole in base_polygon.interiors:
        x, y = hole.xy
        ax.fill(x, y, alpha=1, edgecolor='black', facecolor='white', linewidth=2)
    
    for name, rotation, x, y in placements:
        piece_geom, piece_color = get_tetrominoe_data(name)
        
        if piece_geom and piece_color:
            transformed = transform_piece(piece_geom, int(rotation), x, y)
            
            x_coords, y_coords = transformed.exterior.xy
            ax.fill(x_coords, y_coords, 
                   alpha=piece_color[3], 
                   edgecolor='black', 
                   facecolor=piece_color[:3], 
                   linewidth=2)
            
            centroid = transformed.centroid
            ax.text(centroid.x, centroid.y, name, 
                   ha='center', va='center', 
                   fontsize=14, fontweight='bold', 
                   color='white',
                   bbox=dict(boxstyle="round,pad=0.2", 
                           facecolor='black', alpha=0.7))
    
    plt.title(title, fontsize=18, fontweight='bold', pad=20)
    plt.xlabel('X Coordinate', fontsize=14)
    plt.ylabel('Y Coordinate', fontsize=14)
    plt.grid(True, alpha=0.3, linestyle='--')
    plt.axis('equal')
    
    ax.set_xlim(-0.5, 6.5)
    ax.set_ylim(-0.5, 5.5)
    ax.set_xticks(range(0, 7))
    ax.set_yticks(range(0, 6))
    
    plt.tight_layout()
    
    os.makedirs('images/backtracking_solutions', exist_ok=True)
    
    plt.savefig('images/backtracking_solutions/tetris_solution.png', dpi=300, bbox_inches='tight')
    plt.close()

def visualize_tetris_solution(tetris_output_str, title="Backtracking Solution"):
    """Complete workflow: parse tetris output and visualize solution"""
    
    # Parse the tetris output
    polygon_wkt, placements = parse_tetris_output(tetris_output_str)
    
    if polygon_wkt and placements:
        plot_tetris_solution_from_db(polygon_wkt, placements, title)
        print(f"Successfully plotted solution with {len(placements)} pieces")
    else:
        print("Failed to parse tetris output")

tetris_output = '''All pieces placed successfully!
Solution found!
Board state:
  POLYGON((0 0,6 0,6 5,0 5,0 0),(2 1,3 1,3 2,2 2,2 1),(2 3,3 3,3 4,2 4,2 3))
Placed pieces:
  [placed(Z,(0,2),0),placed(S,(3,4),270),placed(L,(4,0),90),placed(J,(4,0),0),placed(T,(2,0),90),placed(O,(4,3),0),placed(I,(0,4),0)]
                          true'''

visualize_tetris_solution(tetris_output, "Backtracking Solution")

conn.close()