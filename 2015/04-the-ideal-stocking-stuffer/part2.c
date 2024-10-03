/*
Day 4: The Ideal Stocking Stuffer (Part 2)
https://adventofcode.com/2015/day/4
*/

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <CommonCrypto/CommonDigest.h>

void md5_hash(char *data, char *hash);

int main(void)
{
    char hash[(CC_MD5_DIGEST_LENGTH * 2) + 1];
    const char key[] = "ckczppom";

    bool adventcoin_mined = false;
    unsigned int i = 1;

    while (!adventcoin_mined)
    {
        char secret[BUFSIZ];
        char suffix[BUFSIZ];

        strcpy(secret, key);
        sprintf(suffix, "%d", i);
        strcat(secret, suffix);

        md5_hash(secret, hash);

        if (strncmp(hash, "000000", 6) == 0)
        {
            break;
        }

        i++;
    }

    printf("Answer: %d\n", i);

    return 0;
}

void md5_hash(char *data, char *hash)
{
    unsigned char digest[CC_MD5_DIGEST_LENGTH];

    CC_MD5(data, strlen(data), digest);

    for (int i = 0; i < CC_MD5_DIGEST_LENGTH; i++)
    {
        sprintf(&(hash[i * 2]), "%02x", digest[i]);
    }
}