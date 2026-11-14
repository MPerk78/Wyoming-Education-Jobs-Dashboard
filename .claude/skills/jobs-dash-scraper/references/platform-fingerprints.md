# Platform fingerprints

`scripts/fingerprint_platform.py` checks for these automatically. This table is the reasoning behind it, and tells you which existing scraper to reach for. A signature in the HTML is evidence, not proof. A site can embed an old link to a dead tenant (Butte's AppliTrack link outlived the tenant), so always confirm that the scraper returns real postings for the slug.

## Applicant-tracking systems (structured; prefer these, zero new parser code)

| Platform | Signature | Slug / ID to extract | MT scraper | WY scraper |
|---|---|---|---|---|
| Frontline **AppliTrack** | `applitrack.com/<slug>/onlineapp` | `<slug>` | `fetch_applitrack_postings(Slug)` | `fetch_applitrack_postings(tenant_path)` |
| **SchoolSpring** | `<sub>.schoolspring.com`, `api.schoolspring.com` | `<sub>.schoolspring.com` | `fetch_schoolspring_postings` | same |
| **TedK12** | `tedk12.com` | the board URL | `fetch_tedk12_postings` | same |
| **Tyler Portico** | `<sub>.tylerportico.com` | `<sub>` | `fetch_tylerportico_postings(Slug, District)` | (none yet) |
| **Red Rover Hiring** | `jobs.redroverk12.com`, `redroverk12.com/org/<id>` | org id | `fetch_redrover_postings` (chromote) | `fetch_redrover_postings(org_id, slug)` |
| PeopleAdmin (HE) | `*.peopleadmin.com`, `/postings/search` | Atom feed `…/postings/search.atom` | `fetch_peopleadmin_atom` | same |
| NEOGOV | `governmentjobs.com`, `schooljobs.com`, `attract.neoed.com` | agency slug | `fetch_neogov_postings` | same |
| JazzHR | `applytojob.com` | sub | `fetch_jazzhr_postings` | — |
| Paycom | `paycomonline.net` | client key | `fetch_paycom_postings` | — |
| ADP WFN | `workforcenow.adp.com` | cid | split-feed, see `direct_api_scrapers.R` | — |
| isolved Hire | `<sub>.isolvedhire.com` | sub | see `direct_api_scrapers.R` | — |
| Paylocity | `recruiting.paylocity.com` | company GUID | — | `fetch_paylocity_jobs` |
| SimpleHire / Oracle Cloud | `simplehire.com`, `*.oraclecloud.com` | — | — | HE-specific |

The most common win: a district's own "Employment" page is just a link out to one of the rows above. **Follow outbound links**, because the ATS is the target and the district page isn't.

## District CMSs (heuristic; the employment page is content on the district site)

| CMS | Signature | Notes |
|---|---|---|
| **Apptegy** (Thrillshare) | `apptegy.net`, `thrillshare`, `5il.co`, `aptg.co`, `/o/<code>/page/` URLs | JS-rendered: parse chromote `innerText`, and there may be an embedded JSON payload (`decode_apptegy_pagebuilder_nodes()` in MT). Assets must load from `*.apptegy.net` / `5il.co`. MT: `APPTEGY_DISTRICT_SCRAPERS`; WY: `apptegy` misc platform. |
| Finalsite | `finalsite`, `fs-` class prefixes, `resources.finalsite.net` | Often confused with Apptegy, so check the footer. |
| Edlio | `edlio`, `edliocdn` | WY has `parse_edlio_postings`. |
| SchoolBlocks | `schoolblocks` | WY `parse_schoolblocks_postings(widget_title=…)`. |
| SmartSites | `smartsites`, `/\d+_\d+` page paths | WY `parse_smartsites_postings`. |
| Educational Networks | `educationalnetworks.net` | WY `parse_educational_networks_postings`. |
| WordPress | `wp-content`, `wp-json` | WY `parse_wordpress_postings`. Look at the `wp-json` REST API before scraping HTML. |
| Google Sites | `sites.google.com` | JS-rendered, innerText. MT Cut Bank, WY `googlesites`. |
| Blackboard / SchoolMessenger | `schoolwires`, `blackboard`, `schoolmessenger` | No parser yet in either repo. |
| SharpSchool, rSchoolToday, Weebly, Wix, Squarespace | name in asset URLs | No parser yet. Judge per page. |

## Red flags (usually decline or recheck later)
- `docs.google.com` / `drive.google.com` links as the only "postings" (often sign-in-walled, e.g. Victor).
- Only `.pdf` links named like "Application", "Classified Application", "Certified Application".
- Postings only in an image or a scanned PDF (OCR group).
- A page body under ~1 KB of text with no `<script>` hydration: genuinely empty, not JS-rendered.
