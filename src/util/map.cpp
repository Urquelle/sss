struct Map {
    void **vals;
    void **keys;
    size_t len;
    size_t cap;
};

void *
map_get(Map *map, void *key) {
    if (map->len == 0) {
        return NULL;
    }

    assert(IS_POW2(map->cap));
    size_t i = (size_t)ptr_hash(key);
    assert(map->len < map->cap);

    for (;;) {
        i &= map->cap - 1;

        if ( map->keys[i] == key ) {
            return map->vals[i];
        } else if ( !map->keys[i] ) {
            return NULL;
        }
        i++;
    }

    return NULL;
}

void map_put(Map *map, void *key, void *val);

void
map_grow(Map *map, size_t new_cap) {
    new_cap = MAX(16, new_cap);
    Map new_map = {};

    new_map.keys = (void **)urq_calloc(new_cap, sizeof(void *));
    new_map.vals = (void **)urq_alloc(new_cap * sizeof(void *));
    new_map.cap  = new_cap;

    for ( size_t i = 0; i < map->cap; i++ ) {
        if ( map->keys[i] ) {
            map_put(&new_map, map->keys[i], map->vals[i]);
        }
    }

    urq_dealloc(map->keys);
    urq_dealloc(map->vals);
    *map = new_map;
}

void
map_put(Map *map, void *key, void *val) {
    assert(key);
    assert(val);

    if (2*map->len >= map->cap) {
        map_grow(map, 2*map->cap);
    }

    assert(2*map->len < map->cap);
    assert(IS_POW2(map->cap));

    size_t i = (size_t)ptr_hash(key);
    for (;;) {
        i &= map->cap - 1;

        if ( !map->keys[i] ) {
            map->len++;
            map->keys[i] = key;
            map->vals[i] = val;

            return;
        } else if ( map->keys[i] == key ) {
            map->vals[i] = val;

            return;
        }

        i++;
    }
}

void
map_reset(Map *map) {
    for ( int i = 0; i < map->cap; ++i ) {
        if ( map->keys[i] ) {
            ((char **)map->keys)[i] = 0;
        }
    }
}

