import psycopg2
from shapely import wkb 
import matplotlib.pyplot as plt

conn = psycopg2.connect(
    dbname = 'postgres',
    user = 'postgres',
    password = 'BD2025',
    host = 'localhost',
    port = '5432'
)

cur = conn.cursor()
cur.execute("""SELECT s.name AS solution_name, 
            s.board, 
            ST_Collect(DISTINCT s.tetro) AS tetros,
            array_agg(DISTINCT t.name) AS tetros_names,
            array_agg(DISTINCT t.color) AS tetros_colors
            FROM solutions s 
            JOIN tetrominoes t
            ON ST_Relate(s.tetro, t.geom, '*********')
            GROUP BY s.name, s.board;""")

rows = cur.fetchall()

for i, (solution_name, board_wkb, tetros_wkb, tetro_names, tetro_colors) in enumerate(rows, start=1):
    board = wkb.loads(board_wkb)       
    tetros = wkb.loads(tetros_wkb)       

    fig, ax = plt.subplots(figsize=(6, 5))
    
    x, y = board.exterior.xy
    ax.fill(x, y, alpha=1, edgecolor='black', linewidth=5, facecolor='lightgray')
    for hole in board.interiors:
        x, y = hole.xy
        ax.fill(x, y, alpha=1, edgecolor='black', linewidth=5, facecolor='white')


    for tetro, color, name in zip(tetros.geoms, tetro_colors, tetro_names):
        
        r, g, b, a = map(int, color.split(','))
        rgba_color = (r / 255, g / 255, b / 255)
        #print(rgba_color)
        alpha_value = a / 255  
        
        print(name)
        print(color)
        print(tetro)

        x, y = tetro.exterior.xy
        ax.fill(x, y, alpha=alpha_value, edgecolor='black', linewidth=3, facecolor=rgba_color)

        centroid = tetro.centroid
        ax.text(centroid.x, centroid.y, name, color='black', ha='center', va='center', fontsize=8)


    plt.xticks(range(0, 7))
    plt.yticks(range(0, 6))
    plt.title('{}'.format(name))
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.grid(True)
    plt.axis('equal')
    plt.show()