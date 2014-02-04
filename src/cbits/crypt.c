
# define _XOPEN_SOURCE
# include <unistd.h>
# include <string.h>

# include "crypt.h"

/*
 * Call the POSIX crypt() function
 *
 * The crypt() function is not reentrant, so this helper allows us to copy the
 * results out while the Haskell runtime has blocked all other threads.
 *
 * To simplify memory management, we perform allocation for the results on the
 * Haskell side. However, if the provided buffer is too small, we return an
 * indication of the required size.
 */
int crypt_helper(const char *key, const char *salt, char *encrypted, int len)
{
  char *result;
  int req;

  result = crypt(key, salt);
  if (result == 0)
    return -1;

  req = strlen(result) + 1;
  if (req > len)
    return req;

  strcpy(encrypted, result);

  return 0;
}
