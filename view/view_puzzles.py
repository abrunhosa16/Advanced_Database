import psycopg2
from shapely import wkb 
import matplotlib.pyplot as plt
import json

with open('config.json') as f:
    config = json.load(f)

conn = psycopg2.connect(**config)

cur = conn.cursor()

cur.execute('SELECT name, geom FROM puzzles;')

rows = cur.fetchall()

for i, (name, geom_wkb) in enumerate(rows, start=1):
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
    #plt.grid(True, linewidth=5, color='black')
    plt.axis('equal')
    plt.show()
