#include <stdio.h>
#include <string.h>
#include <json-c/json.h>

typedef struct
{
    const char *name;
    const char *version;
    const char *license;
    const char *author;
    const char *maintainer;
    const char *copyright;
    const char *git;
    const char *git_tag;
    const char *description;
} FPM_PACKAGE;

typedef struct
{
    int c;
    FPM_PACKAGE *pkg;
} USRDATA;

static void clear(void *usr)
{
    USRDATA *u = (USRDATA *) usr;
    u->pkg->version = NULL;
    u->pkg->author = NULL;
    u->pkg->license = NULL;
    u->pkg->maintainer = NULL;
    u->pkg->copyright = NULL;
    u->pkg->description = NULL;
    u->pkg->git = NULL;
    u->pkg->git_tag = NULL;
}

static void save(void *usr, const char *key, const char *val)
{
    USRDATA *u = (USRDATA *) usr;

    if (u->c == 0)
    {
        u->pkg->name = val;
        return;
    }

    if (u->c != 2)
    {
        return;
    }

    if (!strcmp(key, "version"))
        u->pkg->version = val;
    else if (!strcmp(key, "author"))
        u->pkg->author = val;
    else if (!strcmp(key, "license"))
        u->pkg->license = val;
    else if (!strcmp(key, "maintainer"))
        u->pkg->maintainer = val;
    else if (!strcmp(key, "copyright"))
        u->pkg->copyright = val;
    else if (!strcmp(key, "description"))
        u->pkg->description = val;
    else if (!strcmp(key, "git"))
        u->pkg->git = val;
    else if (!strcmp(key, "git-tag"))
        u->pkg->git_tag = val;
}

void get_packages(json_object *obj, void (*f)(FPM_PACKAGE, void *), void *usr, void *fortran_ptr)
{
    USRDATA *u = (USRDATA *) usr;

    json_object_object_foreach(obj, key, val)
    {
        if (json_object_get_type(val) == json_type_string)
        {
            save(u, key, json_object_get_string(val));
        }
        else if (json_object_get_type(val) == json_type_object)
        {
            if (u->c == 0) save(u, "name", key);

            if (u->c < 2)
            {
                u->c++;
                get_packages(val, f, usr, fortran_ptr);
                u->c--;
            }
        }
    }

    if (u->c == 2)
    {
        (*f)(*(u->pkg), fortran_ptr);
        clear(u);
    }
}

int parse_json(const char *file, void (*f)(FPM_PACKAGE, void *), void *fortran_ptr)
{
    USRDATA *u = malloc(sizeof(*u));

    if (!u)
        return 1;

    u->c = 0;
    u->pkg = malloc(sizeof(*u->pkg));

    if (!u->pkg)
    {
        free(u);
        return 1;
    }

    memset(u->pkg, 0, sizeof(*u->pkg));

    json_object *root = json_object_from_file(file);

    if (!root)
    {
        free(u->pkg);
        free(u);
        return 2;
    }

    json_object *packages = json_object_object_get(root, "packages");

    if (!packages)
    {
        free(u->pkg);
        free(u);
        return 3;
    }

    get_packages(packages, f, u, fortran_ptr);

    // Clean up
    json_object_put(root);
    free(u->pkg);
    free(u);
    return 0;
}
