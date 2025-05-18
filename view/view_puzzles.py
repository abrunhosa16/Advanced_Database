import psycopg2
from shapely import wkb 
import matplotlib.pyplot as plt
import json

with open('config.json') as f:
    config = json.load(f)

conn = psycopg2.connect(**config)

def show_puzzle_by_name(puzzle_name):
    cur = conn.cursor()
    cur.execute('''
        SELECT name, geom
        FROM puzzles
        WHERE name = %s;
    ''', (puzzle_name,))
    rows = cur.fetchall()

    for name, geom_wkb in rows:
        board = wkb.loads(geom_wkb) 

        fig, ax = plt.subplots(figsize=(6, 6))
        x, y = board.exterior.xy
        ax.fill(x, y, alpha=0.8, edgecolor='none', facecolor='black') 
        ax.plot(x, y, color='black', linewidth=5, alpha=1)
        for hole in board.interiors:
            x, y = hole.xy
            ax.fill(x, y, alpha=1, edgecolor='none', facecolor='white') 
            ax.plot(x, y, color='black', linewidth=5, alpha=1)

        plt.title('{}'.format(name))
        plt.xlabel('X')
        plt.ylabel('Y')
        plt.xticks(range(0, 7))
        plt.yticks(range(0, 6))
        plt.axis('equal')
        plt.show()

show_puzzle_by_name("Board1")
show_puzzle_by_name("Board2")
show_puzzle_by_name("Board3")
show_puzzle_by_name("Board4")
show_puzzle_by_name("Board5")
show_puzzle_by_name("Board6")
show_puzzle_by_name("Board7")