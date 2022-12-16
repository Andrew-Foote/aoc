import heapq

def dijkstra(root, children, cost):
    queue = [(0, root)]
    heapq.heapify(queue)
    seen = set()

    while queue:
        cost_to_node, node = heapq.heappop(queue)
        yield node, cost_to_node

        for child in children(node):
            if child not in seen:
                #seen.add(child)
                cost_to_child = cost_to_node + cost(child, node)
                heapq.heappush(queue, (cost_to_child, child))