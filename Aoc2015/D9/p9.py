#!/usr/bin/env python
"""One-line summary"""

import re
import networkx


def main():
    """Main"""
    graph = networkx.Graph()

    with open('p9-input.txt') as ifh:
        data = ifh.readlines()

    for line in data:
        fromcity, tocity, dist = re.match(
            r'(\w+)\s+to\s+(\w+)\s+=\s+(\d+)', line).groups()

        graph.add_edge(fromcity, tocity, weight=int(dist))

    mindist = float("inf")
    maxdist = 0

    for node in graph.nodes():
        for target in set(graph.nodes()) - {node}:
            for path in networkx.all_simple_paths(graph, source=node,
                                                  target=target):
                if len(path) < len(graph):
                    continue
                dist = 0
                for n in range(len(graph) - 1):
                    dist += graph[path[n]][path[n+1]]['weight']
                if dist < mindist:
                    mindist = dist
                if dist > maxdist:
                    maxdist = dist
    print "Min:", mindist
    print "Max:", maxdist


if __name__ == '__main__':
    main()

