/* See LICENSE file for copyright and license details. */

/* appearance */
static const char font[]   = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-*";
static const char normbordercolor[] = "#070707";
static const char normbgcolor[]     = "#070707";
static const char normfgcolor[]     = "#66a9ba";
static const char selbordercolor[]  = "#111111";
static const char selbgcolor[]      = "#111111";
static const char selfgcolor[]      = "#66a9ba";
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const Bool showbar           = True;     /* False means no bar */
static const Bool topbar            = True;     /* False means bottom bar */

/* tagging */
static const char *tags[] = { ">>SYSTEM", ">_WORK", "NET", "MEDIA ", "MAIL", "IRC", "STEAM"  };

static const Rule rules[] = {
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       0,            True,        -1 },
    { "Firefox",    NULL,       NULL,       1 << 2,       False,       -1 },
	{ "Virtualbox",  NULL,       NULL,       1 << 6,       False,       -1 },
    { "mplayer", NULL,    NULL,        1 << 3,       False,       -1 }, 
};

/* LAyout(s) */
static const float mfact      = 0.55; /* factor of master area size [0.05..0.95] */
static const Bool resizehints = True; /* True means respect size hints in tiled resizals */
static const int nmaster = 2;  /* default number of clients in the master area */

#include "nmaster-ncol.c"

static const Layout layouts[] = {
	/* symbol     arrange function */
    { "-|=",      ntile },
    { "-|-",      nbstack },
	{ "T",      tile },    /* first entry is default */
	{ "N",      NULL },    /* no layout function means floating behavior */
	{ "M",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define MOD4   Mod4Mask
#define TAGKEYS(KEY,TAG) \
    { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
    { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static const char *dmenucmd[]     = { "dmenu_run", "-i", "-fn", font, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *termcmd[]      = { "urxvtc", NULL };
static const char *thunarcmd[]    = { "thunar", NULL };
static const char *rebootcmd[]    = { "sudo", "reboot", NULL };
static const char *shutdowncmd[]  = { "sudo", "shutdown", "-h", "now", NULL };
static const char *webcmd[]       = { "smplayer", NULL };
static const char *mailcmd[]      = { "thunderbird", NULL };
static const char *volupcmd[]     = { "volup", NULL };
static const char *voldowncmd[]   = { "voldown", NULL };
static const char *volwhatcmd[]   = { "volumes_what", NULL };


static Key keys[] = {
    /* modifier                     key        function        argument */
    { MODKEY,                       XK_a,      incnmaster,     {.i = +1 } },
    { MODKEY,                       XK_z,      incnmaster,     {.i = -1 } },
    { MODKEY,                       XK_x,      setnmaster,     {.i = 2 } },
    { MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0] } },
    { MODKEY,                       XK_b,      setlayout,      {.v = &layouts[1] } },
    { MODKEY,                       XK_v,      spawn,          {.v = dmenucmd } },
    { MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
    { MODKEY,                       XK_Up,     spawn,          {.v = volupcmd } },
    { MODKEY,                       XK_Down,   spawn,          {.v = voldowncmd } },
    { MODKEY,                       XK_b,      togglebar,      {0} },
    { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
    { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
    { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
    { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY,                       XK_Tab,    view,           {0} },
    { MODKEY,                       XK_c,      killclient,     {0} },
    { MODKEY,                       XK_space,  setlayout,      {0} },
    { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
    { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
// alt + shift + key
    { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
    { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
    { MODKEY|ShiftMask,             XK_Return, zoom,           {0} }, /* swap master */
    { MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
    { MODKEY|ShiftMask,             XK_h,      setlayout,      {.v = &layouts[0]} }, /* to change layouts horizontal*/
    { MODKEY|ShiftMask,             XK_f,      setlayout,      {.v = &layouts[1]} }, /* floating */
    { MODKEY|ShiftMask,             XK_m,      setlayout,      {.v = &layouts[2]} }, /* monocle */
    { MODKEY|ShiftMask,             XK_v,      setlayout,      {.v = &layouts[3]} }, /* vertical */
    { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
// alt + control + key
    { MODKEY|ControlMask,           XK_r,      spawn,          {.v = rebootcmd } },
    { MODKEY|ControlMask,           XK_s,      spawn,          {.v = shutdowncmd } },
// Super + key
    { MOD4,                         XK_h,      spawn,          {.v = thunarcmd } },
    { MOD4,                         XK_f,      spawn,          {.v = webcmd } },
    { MOD4,                         XK_e,      spawn,          {.v = mailcmd } },
    { MOD4,                         XK_v,      spawn,          {.v = volwhatcmd } },
    TAGKEYS(                        XK_1,                      0)
    TAGKEYS(                        XK_2,                      1)
    TAGKEYS(                        XK_3,                      2)
    TAGKEYS(                        XK_4,                      3)
    TAGKEYS(                        XK_5,                      4)
    TAGKEYS(                        XK_6,                      5)
    TAGKEYS(                        XK_7,                      6)
    TAGKEYS(                        XK_8,                      7)
    TAGKEYS(                        XK_9,                      8)
    { MODKEY|ControlMask,           XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function        argument */
    { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
    { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
    { ClkWinTitle,          0,              Button2,        zoom,           {0} },
    { ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
    { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
    { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
    { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
    { ClkTagBar,            0,              Button1,        view,           {0} },
    { ClkTagBar,            0,              Button3,        toggleview,     {0} },
    { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
    { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
