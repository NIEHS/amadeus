"""Test-only: remove owned copied inputs after publication, then query the saved snapshot."""
from pathlib import Path
import json,hashlib,subprocess
b=Path(__file__).resolve().parents[1];parent=b/'outputs/final-portable';run=Path((parent/'LATEST.txt').read_text().strip());meta=json.loads((run/'source-metadata.json').read_text())
removed=[]
for code in ['tmmx','pr']:
 p=b/'private-cache'/code/f'{code}_2025.nc';assert p.is_file() and hashlib.sha256(p.read_bytes()).hexdigest()==meta[code]['sha256'];p.unlink();removed.append(str(p))
cmd=['Rscript',str(b/'integration/R/database.R'),str(run),'duckdb','SELECT COUNT(*) AS records FROM "AmbientValueAtLocation"']
x=subprocess.run(cmd,capture_output=True,text=True)
(b/'verification/query-without-raw-sources.log').write_text(x.stdout+x.stderr)
assert x.returncode==0 and all(not Path(p).exists() for p in removed)
(b/'reports/source-independent-query.json').write_text(json.dumps(dict(query_succeeded=True,owned_copied_source_files_removed=removed,global_source_cache_untouched=True,scope='Local source-availability test; not independent reviewer/environment replay',run=str(run)),indent=2))
print('Verified saved query succeeds after owned raw copies removed; original source cache unchanged.')
