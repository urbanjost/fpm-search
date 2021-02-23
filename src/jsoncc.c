#include <stdio.h>
#include <string.h>
#include <json-c/json.h>

typedef struct
{
    const char *name;
    const char *version;
    const char *license;
    const char *copyright;
    const char *git;
    const char *git_tag;
    const char *description;
    const char *homepage;

    char **author;
    int author_count;

    char **maintainer;
    int maintainer_count;

    char **categories;
    int categories_count;

    char **keywords;
    int keywords_count;
} FPM_PACKAGE;

typedef struct
{
    int c;
    FPM_PACKAGE pkg;
} USRDATA;

static void save_array_fake(char ***array, const char *value, int *count);

static void clear(void *usr)
{
    USRDATA *u = (USRDATA *) usr;
    int i;

    u->pkg.version = NULL;
    u->pkg.license = NULL;
    u->pkg.copyright = NULL;
    u->pkg.description = NULL;
    u->pkg.homepage = NULL;
    u->pkg.git = NULL;
    u->pkg.git_tag = NULL;

    if (u->pkg.author_count > 0)
    {
        for (i = 0; i < u->pkg.author_count; i++)
            free(u->pkg.author[i]);

        free(u->pkg.author);
    }

    u->pkg.author = NULL;
    u->pkg.author_count = 0;

    if (u->pkg.maintainer_count > 0)
    {
        for (i = 0; i < u->pkg.maintainer_count; i++)
            free(u->pkg.maintainer[i]);

        free(u->pkg.maintainer);
    }

    u->pkg.maintainer = NULL;
    u->pkg.maintainer_count = 0;

    if (u->pkg.categories_count > 0)
    {
        for (i = 0; i < u->pkg.categories_count; i++)
            free(u->pkg.categories[i]);

        free(u->pkg.categories);
    }

    u->pkg.categories = NULL;
    u->pkg.categories_count = 0;

    if (u->pkg.keywords_count > 0)
    {
        for (i = 0; i < u->pkg.keywords_count; i++)
            free(u->pkg.keywords[i]);

        free(u->pkg.keywords);
    }

    u->pkg.keywords = NULL;
    u->pkg.keywords_count = 0;
}

static void save(USRDATA *u, const char *key, const char *val)
{
    if (u->c == 0)
    {
        u->pkg.name = val;
        return;
    }

    if (u->c != 2)
    {
        return;
    }

    if (!strcmp(key, "version"))
        u->pkg.version = val;
    else if (!strcmp(key, "author"))
        save_array_fake(&u->pkg.author, val, &u->pkg.author_count);
    else if (!strcmp(key, "license"))
        u->pkg.license = val;
    else if (!strcmp(key, "maintainer"))
        save_array_fake(&u->pkg.maintainer, val, &u->pkg.maintainer_count);
    else if (!strcmp(key, "copyright"))
        u->pkg.copyright = val;
    else if (!strcmp(key, "description"))
        u->pkg.description = val;
    else if (!strcmp(key, "homepage"))
        u->pkg.homepage = val;
    else if (!strcmp(key, "git"))
        u->pkg.git = val;
    else if (!strcmp(key, "git-tag"))
        u->pkg.git_tag = val;
}

static void save_array_fake(char ***array, const char *value, int *count)
{
    *array = malloc(sizeof(char *) * 1);
    (*array)[0] = strdup(value);
    *count = 1;
}

static void save_array_real(USRDATA *u, const char *key, const json_object *obj, char ***array, int *count)
{
    int n = json_object_array_length(obj);

    *count = n;
    *array = malloc(sizeof(char *) * n);

    for (int i = 0; i < n; i++)
    {
        (*array)[i] = strdup(json_object_get_string(json_object_array_get_idx(obj, i)));
    }
}

static void save_array(USRDATA *u, const char *key, const json_object *obj)
{
    if (u->c != 2)
        return;

    if (!strcmp(key, "author"))
        save_array_real(u, key, obj, &u->pkg.author, &u->pkg.author_count);
    else if (!strcmp(key, "maintainer"))
        save_array_real(u, key, obj, &u->pkg.maintainer, &u->pkg.maintainer_count);
    else if (!strcmp(key, "categories"))
        save_array_real(u, key, obj, &u->pkg.categories, &u->pkg.categories_count);
    else if (!strcmp(key, "keywords"))
        save_array_real(u, key, obj, &u->pkg.keywords, &u->pkg.keywords_count);

    // Ignore all other arrays
}

void get_packages(json_object *obj, void (*f)(FPM_PACKAGE *, void *), void *usr, void *fortran_ptr)
{
    USRDATA *u = (USRDATA *) usr;

    json_object_object_foreach(obj, key, val)
    {
        switch (json_object_get_type(val))
        {
            case json_type_null:
            case json_type_boolean:
            case json_type_double:
            case json_type_int:
            break;

            case json_type_string:
                save(u, key, json_object_get_string(val));
            break;

            case json_type_array:
                save_array(u, key, val);
            break;

            case json_type_object:
            {
                if (u->c == 0) save(u, "name", key);

                if (u->c < 2)
                {
                    u->c++;
                    get_packages(val, f, usr, fortran_ptr);
                    u->c--;
                }
            }
            break;
        }
    }

    if (u->c == 2)
    {
        (*f)(&u->pkg, fortran_ptr);
        clear(u);
    }
}

int parse_json(const char *file, void (*f)(FPM_PACKAGE *, void *), void *fortran_ptr)
{
    USRDATA *u = malloc(sizeof(*u));

    if (!u)
        return 1;

    u->c = 0;
    memset(&u->pkg, 0, sizeof(u->pkg));

    json_object *root = json_object_from_file(file);

    if (!root)
    {
        free(u);
        return 2;
    }

    json_object *packages = json_object_object_get(root, "packages");

    if (!packages)
    {
        free(u);
        return 3;
    }

    get_packages(packages, f, u, fortran_ptr);

    // Clean up
    json_object_put(root);
    free(u);
    return 0;
}
