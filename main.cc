#include <cstring>
#include <iostream>
#include <vector>
 
 
static char *twobyte_memmem(const unsigned char *h, size_t k, const unsigned char *n) {
    uint16_t nw = n[0] << 8 | n[1], hw = h[0] << 8 | h[1];
    for (h += 2, k -= 2; k; k--, hw = hw << 8 | *h++)
        if (hw == nw) return (char *) h - 2;
    return hw == nw ? (char *) h - 2 : nullptr;
}
 
static char *threebyte_memmem(const unsigned char *h, size_t k, const unsigned char *n) {
    uint32_t nw = (uint32_t) n[0] << 24 | n[1] << 16 | n[2] << 8;
    uint32_t hw = (uint32_t) h[0] << 24 | h[1] << 16 | h[2] << 8;
    for (h += 3, k -= 3; k; k--, hw = (hw | *h++) << 8)
        if (hw == nw) return (char *) h - 3;
    return hw == nw ? (char *) h - 3 : nullptr;
}
 
static char *fourbyte_memmem(const unsigned char *h, size_t k, const unsigned char *n) {
    uint32_t nw = (uint32_t) n[0] << 24 | n[1] << 16 | n[2] << 8 | n[3];
    uint32_t hw = (uint32_t) h[0] << 24 | h[1] << 16 | h[2] << 8 | h[3];
    for (h += 4, k -= 4; k; k--, hw = hw << 8 | *h++)
        if (hw == nw) return (char *) h - 4;
    return hw == nw ? (char *) h - 4 : nullptr;
}
 
#define MAX(a, b) ((a)>(b)?(a):(b))
 
#define BITOP(a, b, op) \
 ((a)[(size_t)(b)/(8*sizeof *(a))] op (size_t)1<<((size_t)(b)%(8*sizeof *(a))))
 
static char *twoway_memmem(const unsigned char *h, const unsigned char *z, const unsigned char *n, size_t l) {
    size_t i, ip, jp, k, p, ms, p0, mem, mem0;
    size_t byteset[32 / sizeof(size_t)] = {0};
    size_t shift[256];
 
    /* Computing length of needle and fill shift table */
    for (i = 0; i < l; i++)
        BITOP(byteset, n[i], |=), shift[n[i]] = i + 1;
 
    /* Compute maximal suffix */
    ip = -1;
    jp = 0;
    k = p = 1;
    while (jp + k < l) {
        if (n[ip + k] == n[jp + k]) {
            if (k == p) {
                jp += p;
                k = 1;
            } else k++;
        } else if (n[ip + k] > n[jp + k]) {
            jp += k;
            k = 1;
            p = jp - ip;
        } else {
            ip = jp++;
            k = p = 1;
        }
    }
    ms = ip;
    p0 = p;
 
    /* And with the opposite comparison */
    ip = -1;
    jp = 0;
    k = p = 1;
    while (jp + k < l) {
        if (n[ip + k] == n[jp + k]) {
            if (k == p) {
                jp += p;
                k = 1;
            } else k++;
        } else if (n[ip + k] < n[jp + k]) {
            jp += k;
            k = 1;
            p = jp - ip;
        } else {
            ip = jp++;
            k = p = 1;
        }
    }
    if (ip + 1 > ms + 1) ms = ip;
    else p = p0;
 
    /* Periodic needle? */
    if (memcmp(n, n + p, ms + 1)) {
        mem0 = 0;
        p = MAX(ms, l - ms - 1) + 1;
    } else mem0 = l - p;
    mem = 0;
 
    /* Search loop */
    for (;;) {
        /* If remainder of haystack is shorter than needle, done */
        if (z - h < l) return 0;
 
        /* Check last byte first; advance by shift on mismatch */
        if (BITOP(byteset, h[l - 1], &)) {
            k = l - shift[h[l - 1]];
            if (k) {
                if (k < mem) k = mem;
                h += k;
                mem = 0;
                continue;
            }
        } else {
            h += l;
            mem = 0;
            continue;
        }
 
        /* Compare right half */
        for (k = MAX(ms + 1, mem); k < l && n[k] == h[k]; k++);
        if (k < l) {
            h += k - ms;
            mem = 0;
            continue;
        }
        /* Compare left half */
        for (k = ms + 1; k > mem && n[k - 1] == h[k - 1]; k--);
        if (k <= mem) return (char *) h;
        h += p;
        mem = mem0;
    }
}
 
void *memmem(const void *h0, size_t k, const void *n0, size_t l) {
    const auto *h = static_cast<const unsigned char *>(h0), *n = static_cast<const unsigned char *>(n0);
 
    /* Return immediately on empty needle */
    if (!l) return (void *) h;
 
    /* Return immediately when needle is longer than haystack */
    if (k < l) return nullptr;
 
    /* Use faster algorithms for short needles */
    h = static_cast<const unsigned char *>(memchr(h0, *n, k));
    if (!h || l == 1) return (void *) h;
    k -= h - (const unsigned char *) h0;
    if (k < l) return nullptr;
    if (l == 2) return twobyte_memmem(h, k, n);
    if (l == 3) return threebyte_memmem(h, k, n);
    if (l == 4) return fourbyte_memmem(h, k, n);
 
    return twoway_memmem(h, h + k, n, l);
}
 
 
 
struct value_t {
    std::vector<char> bytes;
    int offset = 0;
};
 
void init_code_last_positions(std::vector<value_t> &values, int *code_last_positions) {
    int position = 0;
    for (auto &value: values) {
        char *bytes = &value.bytes[0];
 
        position += value.offset;
        int size = value.bytes.size();
        for (int m = 0; m < size; m++) {
            code_last_positions[bytes[m]] = position++;
        }
    }
}
 
std::vector<uintptr_t> sunday_mem_search(uintptr_t base, char *buffer, size_t buffer_length, std::vector<value_t> &values, int values_length, int last_value_offset) {
    std::vector<uintptr_t> res;
 
    int code_last_positions[256];
    memset(code_last_positions, -1, 256);
    init_code_last_positions(values, code_last_positions);
    auto diff = buffer_length - values_length;
 
    auto first_value_bytes = &values[0].bytes[0];
    auto first_value_byte_num = values[0].bytes.size();
 
    int values_nums = values.size();
    for (int buffer_index = 0; buffer_index <= diff;) {
 
        auto first_pos = memmem(buffer + buffer_index, buffer_length - buffer_index, first_value_bytes, first_value_byte_num);
        if (!first_pos)
            return;
        buffer_index = ((char *) first_pos - (char *) buffer) + first_value_byte_num;
        int patten_index = first_value_byte_num;
 
        for (int i = 1; i < values_nums; ++i) {
            auto &value = values[i];
 
            buffer_index += value.offset;
            patten_index += value.offset;
 
            char *bytes = &value.bytes[0];
            int size = value.bytes.size();
            int m = 0;
            while (m < size) {
                if (buffer[buffer_index] == bytes[m++]) {
                    buffer_index++;
                    patten_index++;
                } else {
                    int next_index = buffer_index + values_length - patten_index;
                    int code_last_position = code_last_positions[buffer[next_index]];
                    buffer_index =
                            code_last_position == -1 ? next_index - last_value_offset : next_index - code_last_position;
                    break;
                }
            }
            if (m != size)
                break;
        }
        if (patten_index == values_length) {
            std::cout << base + (buffer_index - values_length) << std::endl;
            res.emplace_back(base + (buffer_index - values_length));
        }
    }
    return res;
}
 
int main(int argc, char *argv[]) {
    std::vector<char> string = { 1,  2,  3,   0x21, 0x41, 0, 0, 0x2f, 0x7b, 0, 0, 0, 0x41, 0x00,
                                14, 15, 16,   0x21, 0x41, 7, 9, 0x2f, 0x7b, 5, 3, 4, 0x41, 0x00};
 
    // 等价于 21 41 ?? ?? 2f 7b ?? ?? ?? 41 00, 一共11个字节，最后一个??后第一个字节的offset是9
    std::vector<value_t> patten = {{{0x21, 0x41}},
                                   {{0x2f, 0x7b}, 2},
                                   {{0x41, 0x00}, 3}};
 
    sunday_mem_search(0, &string[0], string.size(), patten, 11, 9);
    return 0;
}