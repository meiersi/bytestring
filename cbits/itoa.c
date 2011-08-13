// /**
// * C++ version 0.4 char* style "itoa":
// * Written by Lukás Chmela
// * Released under GPLv3.
// 
// Adapted to the use as a bounded encoding by Simon Meier
// */
// char* itoa(int base, int x, char* buf) 
// {
//     char *ptr = buf, *ptr1 = buf, *ptr_end, tmp_char;
//     int x_tmp;
// 
//     do {
//         x_tmp = x;
//         x /= base;
//         *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz" [35 + (x_tmp - x * base)];
//     } while ( x );
// 
//     // Apply negative sign
//     if (x_tmp < 0) *ptr++ = '-';
//     ptr_end = ptr--;
//     while (ptr1 < ptr) {
//             tmp_char = *ptr;
//             *ptr--= *ptr1;
//             *ptr1++ = tmp_char;
//     }
//     return ptr_end;
// }
// 
// char* itohex(int x, char* buf) 
// {
//     char *ptr = buf, *ptr1 = buf, *ptr_end, tmp_char;
//     int x_tmp;
// 
//     do {
//         x_tmp = x;
//         x /= 16;
//         *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz" [35 + (x_tmp - x * 16)];
//     } while ( x );
// 
//     // Apply negative sign
//     if (x_tmp < 0) *ptr++ = '-';
//     ptr_end = ptr--;
//     while (ptr1 < ptr) {
//             tmp_char = *ptr;
//             *ptr--= *ptr1;
//             *ptr1++ = tmp_char;
//     }
//     return ptr_end;
// }
// 
// inline char* uint_dec (unsigned int x, char* buf) 
// {
//     char *ptr = buf, *ptr1 = buf, *ptr_end, tmp_char;
//     int x_tmp;
// 
//     do {
//         x_tmp = x;
//         x /= 10;
//         *ptr++ = "0123456789" [x_tmp - x * 10];
//     } while ( x );
// 
//     ptr_end = ptr--;
//     while (ptr1 < ptr) {
//             tmp_char = *ptr;
//             *ptr--= *ptr1;
//             *ptr1++ = tmp_char;
//     }
//     return ptr_end;
// }
//

// TODO:
//   - share encoding tables
//   - solve 32-bit vs. 64 implementation issue.


///////////////////
// Decimal Encoding
///////////////////

// signed integers
char* int_dec (int x, char* buf) 
{
    char *ptr = buf, *ptr1 = buf, *ptr_end, tmp_char;
    int x_tmp;

    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = "9876543210123456789" [9 + (x_tmp - x * 10)];
    } while ( x );

    // Apply negative sign
    if (x_tmp < 0) *ptr++ = '-';
    ptr_end = ptr--;
    while (ptr1 < ptr) {
            tmp_char = *ptr;
            *ptr--= *ptr1;
            *ptr1++ = tmp_char;
    }
    return ptr_end;
}

// signed long ints (64 bit integers)
char* long_int_dec (long long int x, char* buf) 
{
    char *ptr = buf, *ptr1 = buf, *ptr_end, tmp_char;
    long int x_tmp;

    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = "9876543210123456789" [9 + (x_tmp - x * 10)];
    } while ( x );

    // Apply negative sign
    if (x_tmp < 0) *ptr++ = '-';
    ptr_end = ptr--;
    while (ptr1 < ptr) {
            tmp_char = *ptr;
            *ptr--= *ptr1;
            *ptr1++ = tmp_char;
    }
    return ptr_end;
}

// unsigned integers
char* uint_dec (unsigned int x, char* buf) 
{
    char *ptr = buf, *ptr1 = buf, *ptr_end, tmp_char;
    int x_tmp;

    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = "0123456789" [x_tmp - x * 10];
    } while ( x );

    ptr_end = ptr--;
    while (ptr1 < ptr) {
            tmp_char = *ptr;
            *ptr--= *ptr1;
            *ptr1++ = tmp_char;
    }
    return ptr_end;
}

// unsigned long ints
char* long_uint_dec (long long int x, char* buf) 
{
    char *ptr = buf, *ptr1 = buf, *ptr_end, tmp_char;
    long int x_tmp;

    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = "0123456789" [x_tmp - x * 10];
    } while ( x );

    ptr_end = ptr--;
    while (ptr1 < ptr) {
            tmp_char = *ptr;
            *ptr--= *ptr1;
            *ptr1++ = tmp_char;
    }
    return ptr_end;
}


///////////////////////
// Hexadecimal encoding
///////////////////////

// unsigned ints (32 bit words)
char* uint_hex (unsigned int x, char* buf) {
    // write hex representation in reverse order
    char* end = buf;
    do {
        *end++ = "0123456789abcdef" [x & 0xf];
        x >>= 4; 
    } while ( x );
    // store pointer to next free byte
    char* result = end;
    // invert written digits
    end--;
    char* start = buf; 
    while(start < end) {
        char c = *end;
        *end-- = *start;
        *start++ = c;
    }
    return result;
};

// unsigned long ints (64 bit words)
char* long_uint_hex (long long unsigned int x, char* buf) {
    // write hex representation in reverse order
    char* end = buf;
    do {
        *end++ = "0123456789abcdef" [x & 0xf];
        x >>= 4; 
    } while ( x );
    // store pointer to next free byte
    char* result = end;
    // invert written digits
    end--;
    char* start = buf; 
    while(start < end) {
        char c = *end;
        *end-- = *start;
        *start++ = c;
    }
    return result;
};

