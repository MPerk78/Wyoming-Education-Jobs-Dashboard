# Discovering targets to adopt (and rechecking declined ones)

The goal is more districts and colleges with a **direct** scrape. The statewide feeds (Montana OPI, Wyoming WSBA) already show some of their postings, but only as raw location strings that can't be joined to the map, salary, staffing, or vacancy-rate views. Every district moved from "statewide-feed only" to "direct scrape" shows up across the whole dashboard.

## 1. Build the candidate list from the repo, not from memory

The state reference names where the master list and the decline record live. The general method:

1. Start from the **authoritative universe**: Montana's OPI district dropdown (codes < 4000 are real public K-12 LEAs, and ≥ 4000 are Head Start, co-ops, colleges, and private schools), or Wyoming's 48 districts plus its charters. For higher ed, use IPEDS degree-granting institutions in the state.
2. Subtract what's already covered (the registry, plus Wyoming's `misc_district_registry` in `misc_district_scrapers.R`, plus Montana's `llm_extract_targets.csv`).
3. Subtract what's already been investigated and resolved (Montana: every group in `DISTRICT_BACKLOG.md` except "Not yet investigated" and the recheck groups).
4. **Normalize names before you call anything "new."** Real traps in this project:
   - Montana high-school districts are often named after the *county* ("Fergus H S" is Lewistown, "Flathead H S" is Kalispell, "Custer County H S" is Miles City, not the town of Custer).
   - Elementary + high-school districts that share one job board are **one** scrape target (one registry row).
   - Abbreviations ("Spgs" vs "Springs", "Elem", "K-12 Schools").
5. Prioritize: a combined Elem+HS pair or a "K-12 Schools" entry (a real town district) before lone rural elementary feeders, which mostly have no employment page at all.

## 2. Find the real site

Search for the district's own site and employment page (fanning searches out to a subagent is fine here). Then **verify each lead by hand**:

- The page's address, phone, or county matches the Montana/Wyoming district. Same-named towns exist in other states (Group 3 in the Montana backlog is all wrong-state URL traps).
- Look for a real current posting, not just "Employment" in the nav.
- `scripts/fingerprint_platform.py <homepage> <employment-page>` tells you the platform and any outbound ATS links.

Treat a subagent's "I checked these N sites, they're all Finalsite" as a lead only. In this project a research pass once misidentified most of a batch.

## 3. Triage each candidate into exactly one outcome

| Outcome | Criteria | Next |
|---|---|---|
| **Zero-code add** | On a platform the repo already scrapes (AppliTrack, SchoolSpring, TedK12, Tyler Portico, Red Rover, Apptegy, …) | `adopt-new-source.md`, registry row + fixture/test |
| **Bespoke scraper** | Own page with a real structural marker around real postings | `adopt-new-source.md`, new `fetch_*/parse_*` |
| **LLM pilot** (Montana) | Real openings, but only as free prose with no marker | Add to `llm_extract_targets.csv` per `LLM_EXTRACT.md` (shadow only) |
| **Recheck later** | Real page, but only generic forms / empty right now | Record with date |
| **Declined** | Login / Drive folder / OCR-only / no employment page / wrong state / out of scope | Record with the specific reason |

Record every outcome, including declines, with the date and the URL you checked. An undocumented decline gets re-investigated from scratch next session.

## Recheck: revisiting declined districts

Pages change, and a district that only had an application form in August may list real openings in October. When rechecking:

1. Take the "recheck later" and "stale/broken" groups first (Montana backlog Groups 2 and 7). Then the wrong-URL group (3), since the right URL may now be findable. Then the rest, if the user wants.
2. Re-fetch the exact URL recorded, and re-run the fingerprint (a platform migration may have made it a zero-code add).
3. Change the recorded outcome only on live evidence, and update the date even when nothing changed ("rechecked 2026-10-12, still forms only"), so the next pass can skip it.
4. Hiring is seasonal. A district with nothing in October may post in March. Note that in the record, but don't treat it as a reason to build a scraper against an empty page, because a fixture with zero postings can't test a parser.

## Higher ed

Montana's 23 institutions were verified complete on 2026-08-24. The only gaps were 2 defunct schools and 6 out-of-scope trade schools. Wyoming covers its public institutions. So HE discovery is mostly a **recheck**: confirm each in-scope institution's board still resolves on the same platform, and look for any newly accredited, degree-granting institution. Don't re-litigate the documented out-of-scope decisions without asking the user.
