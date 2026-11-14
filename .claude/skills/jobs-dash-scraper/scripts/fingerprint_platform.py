#!/usr/bin/env python3
"""Fingerprint the job/CMS platform behind one or more school web pages.

    python3 fingerprint_platform.py <url> [<url> ...] [--json]

For each URL: fetches raw HTML (no JS), reports platform signatures found,
outbound links to known applicant-tracking systems (with the slug/subdomain
to put in a registry row), employment-looking links worth following, and a
heuristic "needs a JS render" flag. Stdlib only.

It's a lead generator: an embedded link can point at a dead tenant, and a
JS-rendered page can hide everything from a plain fetch. Confirm on the live
page (chromote innerText for JS sites) before writing code.
"""
import json
import re
import ssl
import sys
import urllib.error
import urllib.request
from html.parser import HTMLParser
from urllib.parse import urljoin, urlparse

UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36"

# (platform, kind, regex over raw HTML). kind: "ats" = structured job system
# (existing scraper, usually zero new code), "cms" = district website builder.
SIGNATURES = [
    ("AppliTrack", "ats", r"applitrack\.com/([A-Za-z0-9_-]+)/onlineapp"),
    ("SchoolSpring", "ats", r"([A-Za-z0-9-]+)\.schoolspring\.com"),
    ("TedK12", "ats", r"tedk12\.com[^\"'\s<>]*"),
    ("TylerPortico", "ats", r"([A-Za-z0-9-]+)\.tylerportico\.com"),
    ("RedRoverK12", "ats", r"redroverk12\.com/(?:org/)?([A-Za-z0-9_/-]+)"),
    ("PeopleAdmin", "ats", r"([A-Za-z0-9-]+\.peopleadmin\.com|/postings/search)"),
    ("NEOGOV", "ats", r"(governmentjobs\.com/careers/[A-Za-z0-9_-]+|schooljobs\.com/careers/[A-Za-z0-9_-]+|[A-Za-z0-9-]+\.attract\.neoed\.com)"),
    ("JazzHR", "ats", r"([A-Za-z0-9-]+)\.applytojob\.com"),
    ("Paycom", "ats", r"paycomonline\.net[^\"'\s<>]*"),
    ("ADP WorkforceNow", "ats", r"workforcenow\.adp\.com[^\"'\s<>]*"),
    ("isolved Hire", "ats", r"([A-Za-z0-9-]+)\.isolvedhire\.com"),
    ("Paylocity", "ats", r"recruiting\.paylocity\.com[^\"'\s<>]*"),
    ("SimpleHire", "ats", r"([A-Za-z0-9-]+)\.simplehire\.com"),
    ("OracleCloud", "ats", r"[A-Za-z0-9.-]+\.oraclecloud\.com/hcmUI[^\"'\s<>]*"),
    ("Apptegy", "cms", r"(apptegy\.net|thrillshare|5il\.co|aptg\.co)"),
    ("Finalsite", "cms", r"(finalsite|resources\.finalsite\.net)"),
    ("Edlio", "cms", r"(edliocdn|\bedlio\b)"),
    ("SchoolBlocks", "cms", r"schoolblocks"),
    ("SmartSites", "cms", r"smartsites"),
    ("EducationalNetworks", "cms", r"educationalnetworks\.net"),
    ("Blackboard/SchoolMessenger", "cms", r"(schoolwires|blackboard\.com|schoolmessenger)"),
    ("SharpSchool", "cms", r"sharpschool"),
    ("rSchoolToday", "cms", r"rschooltoday"),
    ("GoogleSites", "cms", r"sites\.google\.com"),
    ("WordPress", "cms", r"(wp-content|wp-json)"),
    ("Wix", "cms", r"(wixstatic|wix\.com)"),
    ("Squarespace", "cms", r"squarespace"),
    ("Weebly", "cms", r"weebly"),
]

RED_FLAGS = [
    ("google-drive/docs link (often sign-in walled)", r"(docs|drive)\.google\.com"),
    ("application-form PDF", r"href=[\"'][^\"']*(application|app[-_]form)[^\"']*\.pdf"),
]

JOB_WORDS = re.compile(r"employ|career|job|vacanc|opening|position|hiring|human[- ]resources", re.I)


class LinkText(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links, self.text, self.scripts = [], [], 0
        self._href, self._buf, self._skip = None, [], 0

    def handle_starttag(self, tag, attrs):
        if tag in ("script", "style", "noscript"):
            self._skip += 1
            self.scripts += tag == "script"
        if tag == "a":
            self._href, self._buf = dict(attrs).get("href"), []

    def handle_endtag(self, tag):
        if tag in ("script", "style", "noscript") and self._skip:
            self._skip -= 1
        if tag == "a" and self._href is not None:
            self.links.append((self._href, " ".join("".join(self._buf).split())))
            self._href = None

    def handle_data(self, data):
        if self._skip:
            return
        self.text.append(data)
        if self._href is not None:
            self._buf.append(data)


def fetch(url, verify=True):
    req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "text/html,*/*"})
    ctx = None if verify else ssl._create_unverified_context()
    with urllib.request.urlopen(req, timeout=25, context=ctx) as r:
        return r.status, r.geturl(), r.read().decode(r.headers.get_content_charset() or "utf-8", "replace")


def fingerprint(url):
    out = {"url": url}
    try:
        try:
            status, final, html = fetch(url)
        except urllib.error.URLError as e:
            # Read-only fingerprinting: a missing local CA bundle (common with
            # python.org builds on macOS) or a school's incomplete cert chain
            # shouldn't hide the page. Say so, since the scraper may hit it too.
            if not isinstance(getattr(e, "reason", None), ssl.SSLError):
                raise
            status, final, html = fetch(url, verify=False)
            out["note_tls"] = f"TLS verify failed ({e.reason}); fetched unverified"
    except urllib.error.HTTPError as e:
        return {**out, "error": f"HTTP {e.code}"}
    except Exception as e:  # DNS, TLS, timeout
        return {**out, "error": f"{type(e).__name__}: {e}"}
    out.update(status=status, final_url=final, html_bytes=len(html))
    bare = lambda u: urlparse(u).netloc.lower().removeprefix("www.")
    if bare(final) != bare(url):
        out["note"] = "redirected to another host -- check this is still the same district"

    hits = []
    for name, kind, rx in SIGNATURES:
        found = sorted({m.group(1) if m.groups() else m.group(0) for m in re.finditer(rx, html, re.I)})
        if found:
            hits.append({"platform": name, "kind": kind, "matches": found[:6]})
    out["platforms"] = hits

    p = LinkText()
    try:
        p.feed(html)
    except Exception:
        pass
    text = " ".join(" ".join(p.text).split())
    out["visible_text_chars"] = len(text)
    out["needs_js_render"] = len(text) < 1500 and p.scripts > 3
    jobs = []
    for href, label in p.links:
        if not href or href.startswith(("mailto:", "tel:", "javascript:", "#")):
            continue
        if JOB_WORDS.search(label) or JOB_WORDS.search(href):
            jobs.append({"text": label[:80], "href": urljoin(final, href)})
    seen, out["job_links"] = set(), []
    for j in jobs:
        if j["href"] not in seen:
            seen.add(j["href"])
            out["job_links"].append(j)
    out["job_links"] = out["job_links"][:15]
    out["red_flags"] = [label for label, rx in RED_FLAGS if re.search(rx, html, re.I)]
    return out


def show(r):
    print(f"== {r['url']}")
    if "error" in r:
        print(f"   ERROR: {r['error']}")
        return
    print(f"   HTTP {r['status']} -> {r['final_url']}  ({r['html_bytes']} bytes html, {r['visible_text_chars']} chars text)")
    if r.get("note_tls"):
        print(f"   TLS:  {r['note_tls']}")
    if r.get("note"):
        print(f"   NOTE: {r['note']}")
    ats = [h for h in r["platforms"] if h["kind"] == "ats"]
    cms = [h for h in r["platforms"] if h["kind"] == "cms"]
    print("   ATS:  " + ("; ".join(f"{h['platform']} {h['matches']}" for h in ats) if ats else "none found"))
    print("   CMS:  " + ("; ".join(h["platform"] for h in cms) if cms else "none recognized"))
    if r["needs_js_render"]:
        print("   JS:   little static text + many scripts -> render with chromote (innerText) before judging")
    for f in r["red_flags"]:
        print(f"   FLAG: {f}")
    if r["job_links"]:
        print("   job-ish links to follow:")
        for j in r["job_links"]:
            print(f"     - {j['text'] or '(no text)'} -> {j['href']}")


if __name__ == "__main__":
    args = [a for a in sys.argv[1:] if a != "--json"]
    if not args:
        sys.exit(__doc__)
    results = [fingerprint(u if "://" in u else "https://" + u) for u in args]
    if "--json" in sys.argv:
        print(json.dumps(results, indent=2))
    else:
        for r in results:
            show(r)
