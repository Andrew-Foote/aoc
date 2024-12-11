from solutions.python.lib.graph import gbfs, gdfs

def test_gdfs_and_gbfs():
    example = {
        's': 'ac',
        'a': 'sbd',
        'b': 'ae',
        'c': 'sdf',
        'd': 'aceg',
        'e': 'bdh',
        'f': 'cg',
        'g': 'dfh',
        'h': 'eg'
    }

    children = lambda node: iter(example[node])
    
    assert list(gdfs('s', children)) == [
        's', 'a', 'b', 'e', 'd', 'c', 'f', 'g', 'h'
    ]

    assert list(gbfs('s', children)) == [
        's', 'a', 'c', 'b', 'd', 'f', 'e', 'g', 'h'
    ]