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
cur.execute("SELECT geom FROM puzzles;")

rows = cur.fetchall()
polygons = [wkb.loads(row[0]) for row in rows]


for i, poly in enumerate(polygons, start=1):
    fig, ax = plt.subplots(figsize=(6, 6))
    
    x, y = poly.exterior.xy
    ax.fill(x, y, alpha=0.8, edgecolor='none', facecolor='black') 
    ax.plot(x, y, color='black', linewidth=5, alpha=1)
    
    for interior in poly.interiors:
        x, y = interior.xy
        ax.fill(x, y, alpha=1, edgecolor='none', facecolor='white') 
        ax.plot(x, y, color='black', linewidth=5, alpha=1) 


    plt.xticks(range(0, 7))
    plt.yticks(range(0, 6))
    plt.title('Puzzle Number: {}'.format(i))
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.grid(True, linewidth=5, color='black')
    plt.axis('equal')
    plt.show()
