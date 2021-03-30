char *
path_normalize(char *path) {
    char *result = path;
    char *c = path;

    while ( *c ) {
        if ( *c == '\\' ) {
            *c = '/';
        }

        c++;
    }

    return result;
}

char *
path_concat(char *path, char *name, char *ext) {
    char *result = NULL;

    char *path_norm = path_normalize(path);
    char *name_norm = path_normalize(name);

    uint32_t path_len = utf8_str_size(path_norm);
    uint32_t name_len = utf8_str_size(name_norm);

    buf_printf(result, "%s%s%s%s", path_norm, (path_norm[path_len-1] == '/' || name_norm[0] == '/' ) ? "" : "/", name_norm, ext);

    return result;
}
