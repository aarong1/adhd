
prevalence_modelled_adult <- tribble(

~trust, ~Adults, ~Prevalence_2.5, ~Prevalence_3.4, ~Transitions_pa, ~Referrals_A,  ~Accepted_A, ~Referrals_B, ~Accepted_B,
  'BHSCT', 280700, 7000, 9500, 91, 372, 260, 933, 653,
  'NHSCT', 370300, 9300, 12600, 120, 490, 343, 1231, 861,
  'SEHSCT', 284200, 7100, 9700, 92, 376, 263, 945, 661,
  'SHSCT', 302000, 7600, 10300, 98, 400, 280, 1004, 703,
  'WHSCT', 233000, 5800, 7900, 75, 308, 216, 774, 542)

# Referals A
# (0.1% of adult population + transitions)
# ~(70% acceptance rate)

# Referals B
# (0.3% of adult population +transitions) 

referrals_modelled_adult_child <- tibble::tribble(
  ~Year, ~Trust, ~Child, ~Adult, ~Total,
    2025, "BHSCT", 1100, 500, 1600,
    2025, "SEHSCT", 950, 450, 1400,
    2025, "SHSCT", 900, 400, 1300,
    2025, "WHSCT", 700, 300, 1000,
    2025, "NHSCT", 1150, 750, 1900,
    2026, "BHSCT", 1155, 525, 1680,
    2026, "SEHSCT", 1000, 470, 1470,
    2026, "SHSCT", 945, 420, 1365,
    2026, "WHSCT", 735, 315, 1050,
    2026, "NHSCT", 1210, 905, 2115,
    2027, "BHSCT", 1210, 555, 1765,
    2027, "SEHSCT", 1050, 495, 1545,
    2027, "SHSCT", 990, 445, 1435,
    2027, "WHSCT", 770, 335, 1105,
    2027, "NHSCT", 1270, 1080, 2350,
    2028, "BHSCT", 1270, 580, 1850,
    2028, "SEHSCT", 1105, 515, 1620,
    2028, "SHSCT", 1040, 470, 1510,
    2028, "WHSCT", 810, 350, 1160,
    2028, "NHSCT", 1335, 1275, 2610,
    2029, "BHSCT", 1335, 605, 1940,
    2029, "SEHSCT", 1160, 540, 1700,
    2029, "SHSCT", 1090, 495, 1585,
    2029, "WHSCT", 850, 370, 1220,
    2029, "NHSCT", 1400, 1485, 2885,
    2030, "BHSCT", 1400, 635, 2035,
    2030, "SEHSCT", 1220, 565, 1785,
    2030, "SHSCT", 1140, 525, 1665,
    2030, "WHSCT", 890, 390, 1280,
    2030, "NHSCT", 1470, 1705, 3175
  )
