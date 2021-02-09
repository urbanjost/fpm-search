#include <time.h>
#include <inttypes.h>
#include <sys/stat.h>

int64_t fileTime(const char *fileName)
{
    struct stat fileinfo;
    stat(fileName, &fileinfo);
    return (int64_t) fileinfo.st_mtime;
}

int64_t now(void)
{
    time_t raw;
    time(&raw);
    return (int64_t) raw;
}
