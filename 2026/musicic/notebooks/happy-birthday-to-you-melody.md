# Happy Birthday to You — melody

*musIC notebook · saved 2026-10-05 09:05*

**Happy Birthday to You — melody** · 3/4 · 120 bpm · each set is `{pitch, duration}` · 25 sets

Each set is written {pitch, duration}:

- **pitch** — MIDI note number: 60 = middle C, 0 = a rest
- **duration** — how long the note lasts; 32 = a quarter note

## The piece's sets

A bar to a line.

```mathematica
{{67,16},{67,16},{69,32},{67,32},
 {72,32},{71,64},
 {67,16},{67,16},{69,32},{67,32},
 {74,32},{72,64},
 {67,16},{67,16},{79,32},{76,32},
 {72,32},{71,32},{69,32},
 {77,16},{77,16},{76,32},{72,32},
 {74,32},{72,64}}
```

## Level 1 — 25 sets → 12 parts · done ✓

### In[1] — sets 1–2 · ✓ matches

```mathematica
IC2[{67,16}]
```

### In[2] — sets 3–4 · ✓ matches

```mathematica
IC(i,0,1)[{69-2i,32}]
```

### In[3] — sets 5–6 · ✓ matches

```mathematica
IC(i,0,1)[{72-i,32+32i}]
```

### In[4] — sets 7–8 · ✓ matches

```mathematica
IC2[{67,16}]
```

### In[5] — sets 9–10 · ✓ matches

```mathematica
IC(i,0,1)[{69-2i,32}]
```

### In[6] — sets 11–12 · ✓ matches

```mathematica
IC(i,0,1)[{74-2i,32+32i}]
```

### In[7] — sets 13–14 · ✓ matches

```mathematica
IC2[{67,16}]
```

### In[8] — sets 15–16 · ✓ matches

```mathematica
IC(i,0,1)[{79-3i,32}]
```

### In[9] — sets 17–19 · ✓ matches

```mathematica
IC(i,0,2)[{72-i(i+1)/2,32}]
```

### In[10] — sets 20–21 · ✓ matches

```mathematica
IC2[{77,16}]
```

### In[11] — sets 22–23 · ✓ matches

```mathematica
IC(i,0,1)[{76-4i,32}]
```

### In[12] — sets 24–25 · ✓ matches

```mathematica
IC(i,0,1)[{74-2i,32+32i}]
```

## Level 2 — 12 items → 3 parts · done ✓

Items (level 1 as it was written):

```mathematica
IC2[{67,16}], IC(i,0,1)[{69-2i,32}], IC(i,0,1)[{72-i,32+32i}], IC2[{67,16}], IC(i,0,1)[{69-2i,32}], IC(i,0,1)[{74-2i,32+32i}], IC2[{67,16}], IC(i,0,1)[{79-3i,32}], IC(i,0,2)[{72-i(i+1)/2,32}], IC2[{77,16}], IC(i,0,1)[{76-4i,32}], IC(i,0,1)[{74-2i,32+32i}]
```

### In[1] — items 1–6 · ✓ matches

```mathematica
IC(j,0,1)[IC(2)[{67,16}], IC(i,0,1)[{69-2i,32}],
 IC(i,0,1)[{72-i+2j-1(i*j),32+32i}]]
```

### In[2] — items 7–11 · ✓ matches

```mathematica
IC(j,0,1){IC(2)[{67+10j,16}], IC(i,0,1)[{79-3i-3j-1(i*j),32}],
 IC(i,0,2-3j)[{72-i(i+1)/2,32}]}
```

### In[3] — items 12 · ✓ matches

```mathematica
{IC(i,0,1)[{74-2i,32+32i}]}
```

## Level 3 — 3 items · 0 of 3 written

Items (level 2 as it was written):

```mathematica
IC(j,0,1)[IC(2)[{67,16}], IC(i,0,1)[{69-2i,32}], IC(i,0,1)[{72-i+2j-1(i*j),32+32i}]], IC(j,0,1){IC(2)[{67+10j,16}], IC(i,0,1)[{79-3i-3j-1(i*j),32}], IC(i,0,2-3j)[{72-i(i+1)/2,32}]}, IC(i,0,1)[{74-2i,32+32i}]
```

---

*The comment below is the exact notebook, for musIC to reopen.*

<!-- musicic:data v1
{"name":"Happy Birthday to You — melody","encoder":"pair","numerator":3,"denominator":4,"tempo":120,"grid":32,"target":{"name":"Happy Birthday to You — melody","encoder":"pair","rows":[[67,16],[67,16],[69,32],[67,32],[72,32],[71,64],[67,16],[67,16],[69,32],[67,32],[74,32],[72,64],[67,16],[67,16],[79,32],[76,32],[72,32],[71,32],[69,32],[77,16],[77,16],[76,32],[72,32],[74,32],[72,64]],"bars":[0,0,0,0,1,1,2,2,2,2,3,3,4,4,4,4,5,5,5,6,6,6,6,7,7],"numerator":3,"denominator":4,"tempo":120,"grid":32},"levels":[{"cells":["IC2[{67,16}]","IC(i,0,1)[{69-2i,32}]","IC(i,0,1)[{72-i,32+32i}]","IC2[{67,16}]","IC(i,0,1)[{69-2i,32}]","IC(i,0,1)[{74-2i,32+32i}]","IC2[{67,16}]","IC(i,0,1)[{79-3i,32}]","IC(i,0,2)[{72-i(i+1)/2,32}]","IC2[{77,16}]","IC(i,0,1)[{76-4i,32}]","IC(i,0,1)[{74-2i,32+32i}]"],"claims":[[0,1],[2,3],[4,5],[6,7],[8,9],[10,11],[12,13],[14,15],[16,17,18],[19,20],[21,22],[23,24]]},{"cells":["IC(j,0,1)[IC(2)[{67,16}], IC(i,0,1)[{69-2i,32}],\n IC(i,0,1)[{72-i+2j-1(i*j),32+32i}]]\n","IC(j,0,1){IC(2)[{67+10j,16}], IC(i,0,1)[{79-3i-3j-1(i*j),32}],\n IC(i,0,2-3j)[{72-i(i+1)/2,32}]}","{IC(i,0,1)[{74-2i,32+32i}]}"],"claims":[[0,1,2,3,4,5],[6,7,8,9,10],[11]],"items":[{"text":"IC2[{67,16}]","rows":[0,1]},{"text":"IC(i,0,1)[{69-2i,32}]","rows":[2,3]},{"text":"IC(i,0,1)[{72-i,32+32i}]","rows":[4,5]},{"text":"IC2[{67,16}]","rows":[6,7]},{"text":"IC(i,0,1)[{69-2i,32}]","rows":[8,9]},{"text":"IC(i,0,1)[{74-2i,32+32i}]","rows":[10,11]},{"text":"IC2[{67,16}]","rows":[12,13]},{"text":"IC(i,0,1)[{79-3i,32}]","rows":[14,15]},{"text":"IC(i,0,2)[{72-i(i+1)/2,32}]","rows":[16,17,18]},{"text":"IC2[{77,16}]","rows":[19,20]},{"text":"IC(i,0,1)[{76-4i,32}]","rows":[21,22]},{"text":"IC(i,0,1)[{74-2i,32+32i}]","rows":[23,24]}]},{"cells":[""],"claims":[[0,1,2]],"items":[{"text":"IC(j,0,1)[IC(2)[{67,16}], IC(i,0,1)[{69-2i,32}], IC(i,0,1)[{72-i+2j-1(i*j),32+32i}]]","rows":[0,1,2,3,4,5,6,7,8,9,10,11]},{"text":"IC(j,0,1){IC(2)[{67+10j,16}], IC(i,0,1)[{79-3i-3j-1(i*j),32}], IC(i,0,2-3j)[{72-i(i+1)/2,32}]}","rows":[12,13,14,15,16,17,18,19,20,21,22]},{"text":"IC(i,0,1)[{74-2i,32+32i}]","rows":[23,24]}]}],"level":2,"order":[1,0]}
-->
