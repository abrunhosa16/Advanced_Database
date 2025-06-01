import psycopg2
from shapely import wkb 
import matplotlib.pyplot as plt
import json
import os

with open('config.json') as f:
    config = json.load(f)

conn = psycopg2.connect(**config)

def show_solutions_for_board(board_name):
    cur = conn.cursor()
    cur.execute('''
        SELECT s.name AS solution_name, 
               s.board, 
               ST_Collect(s.tetro) AS tetros,
               array_agg(s.tname) AS tetros_names,
               array_agg(t.color) AS tetros_colors
        FROM solutions s 
        JOIN tetrominoes t ON s.tname = t.name
        WHERE s.board = (
            SELECT geom FROM puzzles WHERE name = %s
        )
        GROUP BY s.name, s.board;
    ''', (board_name,))

    rows = cur.fetchall()

    os.makedirs('output', exist_ok=True)

    for solution_name, board_wkb, tetros_wkb, tetro_names, tetro_colors in rows:
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
            alpha_value = a / 255  
            x, y = tetro.exterior.xy
            ax.fill(x, y, alpha=alpha_value, edgecolor='black', linewidth=3, facecolor=rgba_color)
            centroid = tetro.centroid
            ax.text(centroid.x, centroid.y, name, color='black', ha='center', va='center', fontsize=8)

        plt.xticks(range(0, 7))
        plt.yticks(range(0, 6))
        plt.title('{}'.format(solution_name))
        plt.xlabel('X')
        plt.ylabel('Y')
        plt.axis('equal')
        
        plt.savefig(f'output/{solution_name}.png')
        plt.close()

show_solutions_for_board("Board1")
show_solutions_for_board("Board2")
show_solutions_for_board("Board3")
show_solutions_for_board("Board4")
show_solutions_for_board("Board5")
show_solutions_for_board("Board6")