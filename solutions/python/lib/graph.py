import heapq

def dfs(root, children):
    # https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html
    # 
    # I always have to look up this blog post since I can never remember where exactly you do the
    # check against the visited set.

    visited = {root}
    stack = [iter(children(root))]

    while stack:
        try:
            node = next(stack[-1])
        except StopIteration:
            del stack[-1]
        else:
            yield node

            if node not in visited:
                visited.add(node)
                stack.append(iter(children(node)))

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