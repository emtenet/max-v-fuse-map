-module(max_v_570z_minimal).

-export([fuses/0]).

-type fuse() :: fuse:fuse().

-spec fuses() -> [fuse()].

fuses() ->
    [46,
     47,
     53,
     54,
     59,
     60,
     69,
     70,
     76,
     77,
     82,
     83,
     89,
     90,
     95,
     99,
     105,
     106,
     111,
     112,
     118,
     119,
     125,
     126,
     134,
     135,
     141,
     142,
     147,
     148,
     154,
     155,
     163,
     164,
     170,
     171,
     177,
     178,
     183,
     184,
     190,
     191,
     200,
     201,
     207,
     208,
     213,
     214,
     220,
     221,
     230,
     231,
     236,
     237,
     243,
     244,
     300,
     567,
     571,
     575,
     589,
     593,
     597,
     601,
     619,
     623,
     627,
     638,
     645,
     649,
     653,
     668,
     675,
     679,
     690,
     694,
     698,
     702,
     721,
     725,
     729,
     743,
     747,
     751,
     755,
     1479,
     1735,
     1991,
     2247,
     3271,
     3527,
     4039,
     4295,
     4899,
     4900,
     4902,
     4904,
     4906,
     5116,
     5118,
     5123,
     5125,
     5126,
     5155,
     5382,
     5411,
     5412,
     5414,
     5416,
     5418,
     5628,
     5630,
     5635,
     5637,
     5638,
     5667,
     5669,
     5671,
     5887,
     5892,
     5894,
     6693,
     6695,
     6911,
     6916,
     7203,
     7204,
     7206,
     7208,
     7210,
     7242,
     7291,
     7343,
     7396,
     7420,
     7422,
     7427,
     7429,
     7430,
     7459,
     7497,
     7546,
     7598,
     7651,
     7686,
     7716,
     7718,
     7720,
     7722,
     7932,
     7934,
     7939,
     7941,
     8240,
     8241,
     8244,
     8245,
     8248,
     8249,
     8252,
     8253,
     8259,
     8260,
     8261,
     8267,
     8268,
     8271,
     8272,
     8275,
     8276,
     8279,
     8280,
     8283,
     8284,
     8292,
     8293,
     8296,
     8297,
     8300,
     8301,
     8304,
     8305,
     8308,
     8309,
     8310,
     8316,
     8317,
     8323,
     8324,
     8327,
     8328,
     8331,
     8332,
     8335,
     8336,
     8341,
     8342,
     8345,
     8346,
     8349,
     8350,
     8356,
     8357,
     8360,
     8361,
     8362,
     8368,
     8369,
     8372,
     8373,
     8376,
     8377,
     8380,
     8381,
     8387,
     8388,
     8394,
     8395,
     8398,
     8399,
     8402,
     8403,
     8406,
     8407,
     8410,
     8411,
     8412,
     8421,
     8422,
     8425,
     8426,
     8429,
     8430,
     8433,
     8434,
     8437,
     8438,
     8496,
     8500,
     8504,
     8508,
     8515,
     8517,
     8518,
     8520,
     8522,
     8524,
     8528,
     8532,
     8536,
     8540,
     8548,
     8552,
     8556,
     8560,
     8564,
     8566,
     8567,
     8569,
     8571,
     8573,
     8580,
     8584,
     8588,
     8592,
     8597,
     8601,
     8605,
     8612,
     8616,
     8618,
     8619,
     8621,
     8623,
     8625,
     8629,
     8633,
     8637,
     8644,
     8650,
     8654,
     8658,
     8662,
     8666,
     8668,
     8669,
     8671,
     8676,
     8678,
     8682,
     8686,
     8690,
     8694,
     8778,
     8827,
     8879,
     8932,
     9507,
     9734,
     9763,
     9990,
     10439,
     10695,
     11207,
     11463,
     12067,
     12068,
     12070,
     12072,
     12074,
     12284,
     12286,
     12291,
     12293,
     12294,
     12323,
     12550,
     12579,
     12580,
     12582,
     12584,
     12586,
     12796,
     12798,
     12803,
     12805,
     12806,
     12835,
     12837,
     12839,
     13055,
     13060,
     13062,
     13861,
     13863,
     14079,
     14084,
     14371,
     14372,
     14374,
     14376,
     14378,
     14410,
     14459,
     14511,
     14564,
     14588,
     14590,
     14595,
     14597,
     14598,
     14627,
     14665,
     14714,
     14766,
     14819,
     14854,
     14884,
     14886,
     14888,
     14890,
     15100,
     15102,
     15107,
     15109,
     15408,
     15409,
     15412,
     15413,
     15416,
     15417,
     15420,
     15421,
     15427,
     15428,
     15429,
     15435,
     15436,
     15439,
     15440,
     15443,
     15444,
     15447,
     15448,
     15451,
     15452,
     15460,
     15461,
     15464,
     15465,
     15468,
     15469,
     15472,
     15473,
     15476,
     15477,
     15478,
     15484,
     15485,
     15491,
     15492,
     15495,
     15496,
     15499,
     15500,
     15503,
     15504,
     15509,
     15510,
     15513,
     15514,
     15517,
     15518,
     15524,
     15525,
     15528,
     15529,
     15530,
     15536,
     15537,
     15540,
     15541,
     15544,
     15545,
     15548,
     15549,
     15555,
     15556,
     15562,
     15563,
     15566,
     15567,
     15570,
     15571,
     15574,
     15575,
     15578,
     15579,
     15580,
     15589,
     15590,
     15593,
     15594,
     15597,
     15598,
     15601,
     15602,
     15605,
     15606,
     15664,
     15668,
     15672,
     15676,
     15683,
     15685,
     15686,
     15688,
     15690,
     15692,
     15696,
     15700,
     15704,
     15708,
     15716,
     15720,
     15724,
     15728,
     15732,
     15734,
     15735,
     15737,
     15739,
     15741,
     15748,
     15752,
     15756,
     15760,
     15765,
     15769,
     15773,
     15780,
     15784,
     15786,
     15787,
     15789,
     15791,
     15793,
     15797,
     15801,
     15805,
     15812,
     15818,
     15822,
     15826,
     15830,
     15834,
     15836,
     15837,
     15839,
     15844,
     15846,
     15850,
     15854,
     15858,
     15862,
     15946,
     15995,
     16047,
     16100,
     16675,
     16902,
     16931,
     17158,
     17607,
     17863,
     18375,
     18631,
     19235,
     19236,
     19238,
     19240,
     19242,
     19452,
     19454,
     19459,
     19461,
     19462,
     19491,
     19718,
     19747,
     19748,
     19750,
     19752,
     19754,
     19964,
     19966,
     19971,
     19973,
     19974,
     20003,
     20005,
     20007,
     20223,
     20228,
     20230,
     21029,
     21031,
     21247,
     21252,
     21539,
     21540,
     21542,
     21544,
     21546,
     21578,
     21627,
     21679,
     21732,
     21756,
     21758,
     21763,
     21765,
     21766,
     21795,
     21833,
     21882,
     21934,
     21987,
     22022,
     22052,
     22054,
     22056,
     22058,
     22268,
     22270,
     22275,
     22277,
     22576,
     22577,
     22580,
     22581,
     22584,
     22585,
     22588,
     22589,
     22595,
     22596,
     22597,
     22603,
     22604,
     22607,
     22608,
     22611,
     22612,
     22615,
     22616,
     22619,
     22620,
     22628,
     22629,
     22632,
     22633,
     22636,
     22637,
     22640,
     22641,
     22644,
     22645,
     22646,
     22652,
     22653,
     22659,
     22660,
     22663,
     22664,
     22667,
     22668,
     22671,
     22672,
     22677,
     22678,
     22681,
     22682,
     22685,
     22686,
     22692,
     22693,
     22696,
     22697,
     22698,
     22704,
     22705,
     22708,
     22709,
     22712,
     22713,
     22716,
     22717,
     22723,
     22724,
     22730,
     22731,
     22734,
     22735,
     22738,
     22739,
     22742,
     22743,
     22746,
     22747,
     22748,
     22757,
     22758,
     22761,
     22762,
     22765,
     22766,
     22769,
     22770,
     22773,
     22774,
     22832,
     22836,
     22840,
     22844,
     22851,
     22853,
     22854,
     22856,
     22858,
     22860,
     22864,
     22868,
     22872,
     22876,
     22884,
     22888,
     22892,
     22896,
     22900,
     22902,
     22903,
     22905,
     22907,
     22909,
     22916,
     22920,
     22924,
     22928,
     22933,
     22937,
     22941,
     22948,
     22952,
     22954,
     22955,
     22957,
     22959,
     22961,
     22965,
     22969,
     22973,
     22980,
     22986,
     22990,
     22994,
     22998,
     23002,
     23004,
     23005,
     23007,
     23012,
     23014,
     23018,
     23022,
     23026,
     23030,
     23114,
     23163,
     23215,
     23268,
     23843,
     24070,
     24099,
     24326,
     24775,
     25031,
     25543,
     25799,
     26403,
     26404,
     26406,
     26408,
     26410,
     26620,
     26622,
     26627,
     26629,
     26630,
     26659,
     26886,
     26915,
     26916,
     26918,
     26920,
     26922,
     27132,
     27134,
     27139,
     27141,
     27142,
     27171,
     27173,
     27175,
     27391,
     27396,
     27398,
     28197,
     28199,
     28415,
     28420,
     28707,
     28708,
     28710,
     28712,
     28714,
     28746,
     28795,
     28847,
     28900,
     28924,
     28926,
     28931,
     28933,
     28934,
     28963,
     29001,
     29050,
     29102,
     29155,
     29190,
     29220,
     29222,
     29224,
     29226,
     29436,
     29438,
     29443,
     29445,
     29744,
     29745,
     29748,
     29749,
     29752,
     29753,
     29756,
     29757,
     29763,
     29764,
     29765,
     29771,
     29772,
     29775,
     29776,
     29779,
     29780,
     29783,
     29784,
     29787,
     29788,
     29796,
     29797,
     29800,
     29801,
     29804,
     29805,
     29808,
     29809,
     29812,
     29813,
     29814,
     29820,
     29821,
     29827,
     29828,
     29831,
     29832,
     29835,
     29836,
     29839,
     29840,
     29845,
     29846,
     29849,
     29850,
     29853,
     29854,
     29860,
     29861,
     29864,
     29865,
     29866,
     29872,
     29873,
     29876,
     29877,
     29880,
     29881,
     29884,
     29885,
     29891,
     29892,
     29898,
     29899,
     29902,
     29903,
     29906,
     29907,
     29910,
     29911,
     29914,
     29915,
     29916,
     29925,
     29926,
     29929,
     29930,
     29933,
     29934,
     29937,
     29938,
     29941,
     29942,
     30000,
     30004,
     30008,
     30012,
     30019,
     30021,
     30022,
     30024,
     30026,
     30028,
     30032,
     30036,
     30040,
     30044,
     30052,
     30056,
     30060,
     30064,
     30068,
     30070,
     30071,
     30073,
     30075,
     30077,
     30084,
     30088,
     30092,
     30096,
     30101,
     30105,
     30109,
     30116,
     30120,
     30122,
     30123,
     30125,
     30127,
     30129,
     30133,
     30137,
     30141,
     30148,
     30154,
     30158,
     30162,
     30166,
     30170,
     30172,
     30173,
     30175,
     30180,
     30182,
     30186,
     30190,
     30194,
     30198,
     30282,
     30331,
     30383,
     30436,
     31011,
     31238,
     31267,
     31494,
     31943,
     32199,
     32711,
     32967,
     33572,
     33576,
     33788,
     33790,
     33795,
     33797,
     33798,
     34054,
     34083,
     34084,
     34088,
     34300,
     34302,
     34307,
     34309,
     34310,
     34339,
     34341,
     34559,
     34564,
     34566,
     35365,
     35367,
     35583,
     35588,
     35875,
     35876,
     35878,
     35880,
     35882,
     35914,
     35963,
     36015,
     36068,
     36092,
     36094,
     36099,
     36101,
     36102,
     36131,
     36169,
     36218,
     36270,
     36323,
     36358,
     36388,
     36390,
     36392,
     36394,
     36604,
     36606,
     36611,
     36613,
     36912,
     36913,
     36916,
     36917,
     36920,
     36921,
     36924,
     36925,
     36931,
     36932,
     36933,
     36939,
     36940,
     36943,
     36944,
     36947,
     36948,
     36951,
     36952,
     36955,
     36956,
     36964,
     36965,
     36968,
     36969,
     36972,
     36973,
     36976,
     36977,
     36980,
     36981,
     36982,
     36988,
     36989,
     36995,
     36996,
     36999,
     37000,
     37003,
     37004,
     37007,
     37008,
     37013,
     37014,
     37017,
     37018,
     37021,
     37022,
     37028,
     37029,
     37032,
     37033,
     37034,
     37040,
     37041,
     37044,
     37045,
     37048,
     37049,
     37052,
     37053,
     37059,
     37060,
     37066,
     37067,
     37070,
     37071,
     37074,
     37075,
     37078,
     37079,
     37082,
     37083,
     37084,
     37093,
     37094,
     37097,
     37098,
     37101,
     37102,
     37105,
     37106,
     37109,
     37110,
     37168,
     37172,
     37176,
     37180,
     37187,
     37189,
     37190,
     37192,
     37194,
     37196,
     37200,
     37204,
     37208,
     37212,
     37220,
     37224,
     37228,
     37232,
     37236,
     37238,
     37239,
     37241,
     37243,
     37245,
     37252,
     37256,
     37260,
     37264,
     37269,
     37273,
     37277,
     37284,
     37288,
     37290,
     37291,
     37293,
     37295,
     37297,
     37301,
     37305,
     37309,
     37316,
     37322,
     37326,
     37330,
     37334,
     37338,
     37340,
     37341,
     37343,
     37348,
     37350,
     37354,
     37358,
     37362,
     37366,
     37450,
     37499,
     37551,
     37604,
     38179,
     38406,
     38435,
     38662,
     39111,
     39367,
     39879,
     40135,
     40739,
     40740,
     40742,
     40744,
     40746,
     40956,
     40958,
     40963,
     40965,
     40966,
     40995,
     41222,
     41251,
     41252,
     41254,
     41256,
     41258,
     41468,
     41470,
     41475,
     41477,
     41478,
     41507,
     41509,
     41511,
     41727,
     41732,
     41734,
     42533,
     42535,
     42751,
     42756,
     43043,
     43044,
     43046,
     43048,
     43050,
     43082,
     43131,
     43183,
     43236,
     43260,
     43262,
     43267,
     43269,
     43270,
     43299,
     43337,
     43386,
     43438,
     43491,
     43526,
     43556,
     43558,
     43560,
     43562,
     43772,
     43774,
     43779,
     43781,
     44080,
     44081,
     44084,
     44085,
     44088,
     44089,
     44092,
     44093,
     44099,
     44100,
     44101,
     44107,
     44108,
     44111,
     44112,
     44115,
     44116,
     44119,
     44120,
     44123,
     44124,
     44132,
     44133,
     44136,
     44137,
     44140,
     44141,
     44144,
     44145,
     44148,
     44149,
     44150,
     44156,
     44157,
     44163,
     44164,
     44167,
     44168,
     44171,
     44172,
     44175,
     44176,
     44181,
     44182,
     44185,
     44186,
     44189,
     44190,
     44196,
     44197,
     44200,
     44201,
     44202,
     44208,
     44209,
     44212,
     44213,
     44216,
     44217,
     44220,
     44221,
     44227,
     44228,
     44234,
     44235,
     44238,
     44239,
     44242,
     44243,
     44246,
     44247,
     44250,
     44251,
     44252,
     44261,
     44262,
     44265,
     44266,
     44269,
     44270,
     44273,
     44274,
     44277,
     44278,
     44336,
     44340,
     44344,
     44348,
     44355,
     44357,
     44358,
     44360,
     44362,
     44364,
     44368,
     44372,
     44376,
     44380,
     44388,
     44392,
     44396,
     44400,
     44404,
     44406,
     44407,
     44409,
     44411,
     44413,
     44420,
     44424,
     44428,
     44432,
     44437,
     44441,
     44445,
     44452,
     44456,
     44458,
     44459,
     44461,
     44463,
     44465,
     44469,
     44473,
     44477,
     44484,
     44490,
     44494,
     44498,
     44502,
     44506,
     44508,
     44509,
     44511,
     44516,
     44518,
     44522,
     44526,
     44530,
     44534,
     44618,
     44667,
     44719,
     44772,
     45347,
     45574,
     45603,
     45830,
     46279,
     46535,
     47047,
     47303,
     47907,
     47908,
     47910,
     47912,
     47914,
     48124,
     48126,
     48131,
     48133,
     48134,
     48163,
     48390,
     48419,
     48420,
     48422,
     48424,
     48426,
     48636,
     48638,
     48643,
     48645,
     48646,
     48675,
     48677,
     48679,
     48895,
     48900,
     48902,
     49701,
     49703,
     49919,
     49924,
     50211,
     50212,
     50214,
     50216,
     50218,
     50250,
     50299,
     50351,
     50404,
     50428,
     50430,
     50435,
     50437,
     50438,
     50467,
     50505,
     50554,
     50606,
     50659,
     50694,
     50724,
     50726,
     50728,
     50730,
     50940,
     50942,
     50947,
     50949,
     51248,
     51249,
     51252,
     51253,
     51256,
     51257,
     51260,
     51261,
     51267,
     51268,
     51269,
     51275,
     51276,
     51279,
     51280,
     51283,
     51284,
     51287,
     51288,
     51291,
     51292,
     51300,
     51301,
     51304,
     51305,
     51308,
     51309,
     51312,
     51313,
     51316,
     51317,
     51318,
     51324,
     51325,
     51331,
     51332,
     51335,
     51336,
     51339,
     51340,
     51343,
     51344,
     51349,
     51350,
     51353,
     51354,
     51357,
     51358,
     51364,
     51365,
     51368,
     51369,
     51370,
     51376,
     51377,
     51380,
     51381,
     51384,
     51385,
     51388,
     51389,
     51395,
     51396,
     51402,
     51403,
     51406,
     51407,
     51410,
     51411,
     51414,
     51415,
     51418,
     51419,
     51420,
     51429,
     51430,
     51433,
     51434,
     51437,
     51438,
     51441,
     51442,
     51445,
     51446,
     51504,
     51508,
     51512,
     51516,
     51523,
     51525,
     51526,
     51528,
     51530,
     51532,
     51536,
     51540,
     51544,
     51548,
     51556,
     51560,
     51564,
     51568,
     51572,
     51574,
     51575,
     51577,
     51579,
     51581,
     51588,
     51592,
     51596,
     51600,
     51605,
     51609,
     51613,
     51620,
     51624,
     51626,
     51627,
     51629,
     51631,
     51633,
     51637,
     51641,
     51645,
     51652,
     51658,
     51662,
     51666,
     51670,
     51674,
     51676,
     51677,
     51679,
     51684,
     51686,
     51690,
     51694,
     51698,
     51702,
     51786,
     51835,
     51887,
     51940,
     52515,
     52742,
     52771,
     52998,
     53447,
     53703,
     54215,
     54471,
     55075,
     55076,
     55078,
     55080,
     55082,
     55292,
     55294,
     55299,
     55301,
     55302,
     55331,
     55558,
     55587,
     55588,
     55590,
     55592,
     55594,
     55804,
     55806,
     55811,
     55813,
     55814,
     55843,
     55845,
     55847,
     56063,
     56068,
     56070,
     56869,
     56871,
     57087,
     57092,
     57379,
     57380,
     57382,
     57384,
     57386,
     57418,
     57467,
     57519,
     57572,
     57596,
     57598,
     57603,
     57605,
     57606,
     57635,
     57673,
     57722,
     57774,
     57827,
     57862,
     57892,
     57894,
     57896,
     57898,
     58108,
     58110,
     58115,
     58117,
     58416,
     58417,
     58420,
     58421,
     58424,
     58425,
     58428,
     58429,
     58435,
     58436,
     58437,
     58443,
     58444,
     58447,
     58448,
     58451,
     58452,
     58455,
     58456,
     58459,
     58460,
     58468,
     58469,
     58472,
     58473,
     58476,
     58477,
     58480,
     58481,
     58484,
     58485,
     58486,
     58492,
     58493,
     58499,
     58500,
     58503,
     58504,
     58507,
     58508,
     58511,
     58512,
     58517,
     58518,
     58521,
     58522,
     58525,
     58526,
     58532,
     58533,
     58536,
     58537,
     58538,
     58544,
     58545,
     58548,
     58549,
     58552,
     58553,
     58556,
     58557,
     58563,
     58564,
     58570,
     58571,
     58574,
     58575,
     58578,
     58579,
     58582,
     58583,
     58586,
     58587,
     58588,
     58597,
     58598,
     58601,
     58602,
     58605,
     58606,
     58609,
     58610,
     58613,
     58614,
     58672,
     58676,
     58680,
     58684,
     58691,
     58693,
     58694,
     58696,
     58698,
     58700,
     58704,
     58708,
     58712,
     58716,
     58724,
     58728,
     58732,
     58736,
     58740,
     58742,
     58743,
     58745,
     58747,
     58749,
     58756,
     58760,
     58764,
     58768,
     58773,
     58777,
     58781,
     58788,
     58792,
     58794,
     58795,
     58797,
     58799,
     58801,
     58805,
     58809,
     58813,
     58820,
     58826,
     58830,
     58834,
     58838,
     58842,
     58844,
     58845,
     58847,
     58852,
     58854,
     58858,
     58862,
     58866,
     58870,
     58954,
     59003,
     59055,
     59108,
     59683,
     59910,
     59939,
     60166,
     60615,
     60871,
     61383,
     61639,
     62244,
     62248,
     62755,
     62756,
     62760,
     63011,
     63013,
     64037,
     64039,
     64547,
     64548,
     64550,
     64552,
     64554,
     64586,
     64635,
     64687,
     64740,
     64803,
     64841,
     64890,
     64942,
     64995,
     65060,
     65062,
     65064,
     65066,
     65584,
     65585,
     65588,
     65589,
     65592,
     65593,
     65596,
     65597,
     65603,
     65604,
     65605,
     65611,
     65612,
     65615,
     65616,
     65619,
     65620,
     65623,
     65624,
     65627,
     65628,
     65636,
     65637,
     65640,
     65641,
     65644,
     65645,
     65648,
     65649,
     65652,
     65653,
     65654,
     65660,
     65661,
     65667,
     65668,
     65671,
     65672,
     65675,
     65676,
     65679,
     65680,
     65685,
     65686,
     65689,
     65690,
     65693,
     65694,
     65700,
     65701,
     65704,
     65705,
     65706,
     65712,
     65713,
     65716,
     65717,
     65720,
     65721,
     65724,
     65725,
     65731,
     65732,
     65738,
     65739,
     65742,
     65743,
     65746,
     65747,
     65750,
     65751,
     65754,
     65755,
     65756,
     65765,
     65766,
     65769,
     65770,
     65773,
     65774,
     65777,
     65778,
     65781,
     65782,
     65840,
     65844,
     65848,
     65852,
     65859,
     65861,
     65862,
     65864,
     65866,
     65868,
     65872,
     65876,
     65880,
     65884,
     65892,
     65896,
     65900,
     65904,
     65908,
     65910,
     65911,
     65913,
     65915,
     65917,
     65924,
     65928,
     65932,
     65936,
     65941,
     65945,
     65949,
     65956,
     65960,
     65962,
     65963,
     65965,
     65967,
     65969,
     65973,
     65977,
     65981,
     65988,
     65994,
     65998,
     66002,
     66006,
     66010,
     66012,
     66013,
     66015,
     66020,
     66022,
     66026,
     66030,
     66034,
     66038,
     66041,
     66045,
     66052,
     66056,
     66077,
     66084,
     66088,
     66092,
     66097,
     66101,
     66105,
     66109,
     66125,
     66129,
     66133,
     66250,
     66299,
     66351,
     66404,
     66428,
     66435,
     66439,
     66443,
     66458,
     66462,
     66469,
     66473,
     66484,
     66488,
     66492,
     66499,
     66506,
     66510,
     66514,
     67143,
     67363,
     67747,
     67911,
     68679,
     69063,
     69447,
     69831,
     70215,
     70983,
     71203,
     71204,
     71206,
     71208,
     71210,
     71573,
     71575,
     71577,
     71579,
     71580,
     71587,
     71964,
     71971,
     71972,
     71974,
     71976,
     71978,
     72341,
     72343,
     72345,
     72347,
     72348,
     72355,
     72357,
     72359,
     72728,
     72730,
     72732,
     73893,
     73895,
     74264,
     74266,
     74659,
     74660,
     74662,
     74664,
     74666,
     74698,
     74747,
     74799,
     74852,
     74901,
     74953,
     75002,
     75029,
     75031,
     75033,
     75035,
     75036,
     75043,
     75081,
     75130,
     75182,
     75235,
     75284,
     75336,
     75385,
     75420,
     75428,
     75430,
     75432,
     75434,
     75797,
     75799,
     75801,
     75803,
     76208,
     76209,
     76212,
     76213,
     76216,
     76217,
     76220,
     76221,
     76227,
     76228,
     76229,
     76235,
     76236,
     76239,
     76240,
     76243,
     76244,
     76247,
     76248,
     76251,
     76252,
     76260,
     76261,
     76264,
     76265,
     76268,
     76269,
     76272,
     76273,
     76276,
     76277,
     76278,
     76284,
     76285,
     76291,
     76292,
     76295,
     76296,
     76299,
     76300,
     76303,
     76304,
     76309,
     76310,
     76313,
     76314,
     76317,
     76318,
     76324,
     76325,
     76328,
     76329,
     76330,
     76336,
     76337,
     76340,
     76341,
     76344,
     76345,
     76348,
     76349,
     76355,
     76356,
     76362,
     76363,
     76366,
     76367,
     76370,
     76371,
     76374,
     76375,
     76378,
     76379,
     76380,
     76389,
     76390,
     76393,
     76394,
     76397,
     76398,
     76401,
     76402,
     76405,
     76406,
     76411,
     76412,
     76415,
     76419,
     76422,
     76423,
     76426,
     76427,
     76430,
     76431,
     76432,
     76438,
     76439,
     76442,
     76443,
     76446,
     76447,
     76453,
     76454,
     76457,
     76458,
     76463,
     76464,
     76467,
     76468,
     76471,
     76472,
     76475,
     76476,
     76479,
     76483,
     76484,
     76490,
     76491,
     76494,
     76495,
     76498,
     76499,
     76502,
     76503,
     76506,
     76507,
     76515,
     76516,
     76519,
     76520,
     76523,
     76524,
     76527,
     76528,
     76531,
     76532,
     76533,
     76539,
     76540,
     76543,
     76547,
     76550,
     76551,
     76554,
     76555,
     76558,
     76559,
     76592,
     76596,
     76600,
     76604,
     76611,
     76613,
     76614,
     76616,
     76618,
     76620,
     76624,
     76628,
     76632,
     76636,
     76644,
     76648,
     76652,
     76656,
     76660,
     76662,
     76663,
     76665,
     76667,
     76669,
     76676,
     76680,
     76684,
     76688,
     76693,
     76697,
     76701,
     76708,
     76712,
     76714,
     76715,
     76717,
     76719,
     76721,
     76725,
     76729,
     76733,
     76740,
     76746,
     76750,
     76754,
     76758,
     76762,
     76764,
     76765,
     76767,
     76772,
     76774,
     76778,
     76782,
     76786,
     76790,
     76795,
     76799,
     76806,
     76810,
     76814,
     76816,
     76817,
     76819,
     76821,
     76823,
     76827,
     76831,
     76838,
     76842,
     76847,
     76851,
     76855,
     76859,
     76863,
     76868,
     76869,
     76871,
     76873,
     76875,
     76879,
     76883,
     76887,
     76891,
     76899,
     76903,
     76907,
     76911,
     76915,
     76917,
     76918,
     76920,
     76922,
     76924,
     76931,
     76935,
     76939,
     76943,
     77002,
     77051,
     77103,
     77156,
     77205,
     77257,
     77306,
     78115,
     78492,
     78499,
     78876,
     79431,
     79815,
     80583,
     80967,
     81955,
     81956,
     81958,
     81960,
     81962,
     82325,
     82327,
     82329,
     82331,
     82332,
     82339,
     82716,
     82723,
     82724,
     82726,
     82728,
     82730,
     83093,
     83095,
     83097,
     83099,
     83100,
     83107,
     83109,
     83111,
     83480,
     83482,
     83484,
     84645,
     84647,
     85016,
     85018,
     85411,
     85412,
     85414,
     85416,
     85418,
     85450,
     85499,
     85551,
     85604,
     85653,
     85705,
     85754,
     85781,
     85783,
     85785,
     85787,
     85788,
     85795,
     85833,
     85882,
     85934,
     85987,
     86036,
     86088,
     86137,
     86172,
     86180,
     86182,
     86184,
     86186,
     86549,
     86551,
     86553,
     86555,
     86960,
     86961,
     86964,
     86965,
     86968,
     86969,
     86972,
     86973,
     86979,
     86980,
     86981,
     86987,
     86988,
     86991,
     86992,
     86995,
     86996,
     86999,
     87000,
     87003,
     87004,
     87012,
     87013,
     87016,
     87017,
     87020,
     87021,
     87024,
     87025,
     87028,
     87029,
     87030,
     87036,
     87037,
     87043,
     87044,
     87047,
     87048,
     87051,
     87052,
     87055,
     87056,
     87061,
     87062,
     87065,
     87066,
     87069,
     87070,
     87076,
     87077,
     87080,
     87081,
     87082,
     87088,
     87089,
     87092,
     87093,
     87096,
     87097,
     87100,
     87101,
     87107,
     87108,
     87114,
     87115,
     87118,
     87119,
     87122,
     87123,
     87126,
     87127,
     87130,
     87131,
     87132,
     87141,
     87142,
     87145,
     87146,
     87149,
     87150,
     87153,
     87154,
     87157,
     87158,
     87163,
     87164,
     87167,
     87171,
     87174,
     87175,
     87178,
     87179,
     87182,
     87183,
     87184,
     87190,
     87191,
     87194,
     87195,
     87198,
     87199,
     87205,
     87206,
     87209,
     87210,
     87215,
     87216,
     87219,
     87220,
     87223,
     87224,
     87227,
     87228,
     87231,
     87235,
     87236,
     87242,
     87243,
     87246,
     87247,
     87250,
     87251,
     87254,
     87255,
     87258,
     87259,
     87267,
     87268,
     87271,
     87272,
     87275,
     87276,
     87279,
     87280,
     87283,
     87284,
     87285,
     87291,
     87292,
     87295,
     87299,
     87302,
     87303,
     87306,
     87307,
     87310,
     87311,
     87344,
     87348,
     87352,
     87356,
     87363,
     87365,
     87366,
     87368,
     87370,
     87372,
     87376,
     87380,
     87384,
     87388,
     87396,
     87400,
     87404,
     87408,
     87412,
     87414,
     87415,
     87417,
     87419,
     87421,
     87428,
     87432,
     87436,
     87440,
     87445,
     87449,
     87453,
     87460,
     87464,
     87466,
     87467,
     87469,
     87471,
     87473,
     87477,
     87481,
     87485,
     87492,
     87498,
     87502,
     87506,
     87510,
     87514,
     87516,
     87517,
     87519,
     87524,
     87526,
     87530,
     87534,
     87538,
     87542,
     87547,
     87551,
     87558,
     87562,
     87566,
     87568,
     87569,
     87571,
     87573,
     87575,
     87579,
     87583,
     87590,
     87594,
     87599,
     87603,
     87607,
     87611,
     87615,
     87620,
     87621,
     87623,
     87625,
     87627,
     87631,
     87635,
     87639,
     87643,
     87651,
     87655,
     87659,
     87663,
     87667,
     87669,
     87670,
     87672,
     87674,
     87676,
     87683,
     87687,
     87691,
     87695,
     87754,
     87803,
     87855,
     87908,
     87957,
     88009,
     88058,
     88867,
     89244,
     89251,
     89628,
     90183,
     90567,
     91335,
     91719,
     92707,
     92708,
     92710,
     92712,
     92714,
     93077,
     93079,
     93081,
     93083,
     93084,
     93091,
     93468,
     93475,
     93476,
     93478,
     93480,
     93482,
     93845,
     93847,
     93849,
     93851,
     93852,
     93859,
     93861,
     93863,
     94232,
     94234,
     94236,
     95397,
     95399,
     95768,
     95770,
     96163,
     96164,
     96166,
     96168,
     96170,
     96202,
     96251,
     96303,
     96356,
     96405,
     96457,
     96506,
     96533,
     96535,
     96537,
     96539,
     96540,
     96547,
     96585,
     96634,
     96686,
     96739,
     96788,
     96840,
     96889,
     96924,
     96932,
     96934,
     96936,
     96938,
     97301,
     97303,
     97305,
     97307,
     97712,
     97713,
     97716,
     97717,
     97720,
     97721,
     97724,
     97725,
     97731,
     97732,
     97733,
     97739,
     97740,
     97743,
     97744,
     97747,
     97748,
     97751,
     97752,
     97755,
     97756,
     97764,
     97765,
     97768,
     97769,
     97772,
     97773,
     97776,
     97777,
     97780,
     97781,
     97782,
     97788,
     97789,
     97795,
     97796,
     97799,
     97800,
     97803,
     97804,
     97807,
     97808,
     97813,
     97814,
     97817,
     97818,
     97821,
     97822,
     97828,
     97829,
     97832,
     97833,
     97834,
     97840,
     97841,
     97844,
     97845,
     97848,
     97849,
     97852,
     97853,
     97859,
     97860,
     97866,
     97867,
     97870,
     97871,
     97874,
     97875,
     97878,
     97879,
     97882,
     97883,
     97884,
     97893,
     97894,
     97897,
     97898,
     97901,
     97902,
     97905,
     97906,
     97909,
     97910,
     97915,
     97916,
     97919,
     97923,
     97926,
     97927,
     97930,
     97931,
     97934,
     97935,
     97936,
     97942,
     97943,
     97946,
     97947,
     97950,
     97951,
     97957,
     97958,
     97961,
     97962,
     97967,
     97968,
     97971,
     97972,
     97975,
     97976,
     97979,
     97980,
     97983,
     97987,
     97988,
     97994,
     97995,
     97998,
     97999,
     98002,
     98003,
     98006,
     98007,
     98010,
     98011,
     98019,
     98020,
     98023,
     98024,
     98027,
     98028,
     98031,
     98032,
     98035,
     98036,
     98037,
     98043,
     98044,
     98047,
     98051,
     98054,
     98055,
     98058,
     98059,
     98062,
     98063,
     98096,
     98100,
     98104,
     98108,
     98115,
     98117,
     98118,
     98120,
     98122,
     98124,
     98128,
     98132,
     98136,
     98140,
     98148,
     98152,
     98156,
     98160,
     98164,
     98166,
     98167,
     98169,
     98171,
     98173,
     98180,
     98184,
     98188,
     98192,
     98197,
     98201,
     98205,
     98212,
     98216,
     98218,
     98219,
     98221,
     98223,
     98225,
     98229,
     98233,
     98237,
     98244,
     98250,
     98254,
     98258,
     98262,
     98266,
     98268,
     98269,
     98271,
     98276,
     98278,
     98282,
     98286,
     98290,
     98294,
     98299,
     98303,
     98310,
     98314,
     98318,
     98320,
     98321,
     98323,
     98325,
     98327,
     98331,
     98335,
     98342,
     98346,
     98351,
     98355,
     98359,
     98363,
     98367,
     98372,
     98373,
     98375,
     98377,
     98379,
     98383,
     98387,
     98391,
     98395,
     98403,
     98407,
     98411,
     98415,
     98419,
     98421,
     98422,
     98424,
     98426,
     98428,
     98435,
     98439,
     98443,
     98447,
     98506,
     98555,
     98607,
     98660,
     98709,
     98761,
     98810,
     99619,
     99996,
     100003,
     100380,
     100935,
     101319,
     102087,
     102471,
     105015,
     105019,
     105023,
     105037,
     105041,
     105045,
     105067,
     105071,
     105075,
     105086,
     105093,
     105097,
     105116,
     105123,
     105127,
     105138,
     105142,
     105146,
     105169,
     105173,
     105177,
     105191,
     105195,
     105199,
     105221,
     105225,
     105229,
     105240,
     105244,
     105251,
     105270,
     105274,
     105278,
     105292,
     105296,
     105300,
     105322,
     105326,
     105330,
     105341,
     105348,
     105352,
     105774,
     105775,
     105781,
     105782,
     105787,
     105788,
     105797,
     105798,
     105804,
     105805,
     105810,
     105811,
     105823,
     105827,
     105833,
     105834,
     105839,
     105840,
     105846,
     105847,
     105853,
     105854,
     105862,
     105863,
     105875,
     105876,
     105882,
     105883,
     105891,
     105892,
     105898,
     105899,
     105905,
     105906,
     105911,
     105912,
     105928,
     105929,
     105935,
     105936,
     105941,
     105942,
     105948,
     105949,
     105958,
     105959,
     105964,
     105965,
     105977,
     105978,
     105987,
     105988,
     105993,
     105994,
     106000,
     106001,
     106007,
     106008,
     106013,
     106014,
     106029,
     106030,
     106036,
     106037,
     106042,
     106043,
     106052,
     106053,
     106059,
     106060,
     106065,
     106066,
     106078,
     106079,
     106088,
     106089,
     106094,
     106095,
     106101,
     106102,
     106108,
     106109,
     106117,
     106118
    ].

