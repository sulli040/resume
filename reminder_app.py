# ✅ 위치·날씨 기반 알림 어플 프로토타입 (Reminder App Prototype)

!pip install schedule geopy requests -q

import os
import time
import json
import atexit
import threading
from datetime import datetime, timedelta
import requests
import schedule
from geopy.distance import geodesic

# ========== 🔧 설정 ==========
WEATHER_API_KEY = os.getenv("WEATHER_API_KEY", "66d7ac96c57f490a87b160434251007")
GEOFENCE_METERS = 80
WEATHER_CHECK_MINUTES = 20
HTTP_TIMEOUT_SEC = 5
DEDUP_MINUTES_DEFAULT = 30
DEDUP_MINUTES_WEATHER = 120
QUIET_HOURS = {"enabled": False, "start_hour": 23, "end_hour": 7}
STATE_PATH = "./reminder_state.json"

# ========== 📋 리마인더 정의 ==========
reminders = [
    {"type": "time", "label": "약 복용", "time": "09:00"},
    {"type": "weather", "label": "우산", "location": "Seoul"},
    {"type": "location", "label": "지갑", "target": (37.3297, 127.1416)},
]

# ========== 💾 상태 관리 ==========
def _load_state():
    if os.path.exists(STATE_PATH):
        try:
            with open(STATE_PATH, "r", encoding="utf-8") as f:
                return json.load(f)
        except Exception:
            pass
    return {"last_notified": {}, "geo_inside": {}}

def _save_state(state):
    try:
        with open(STATE_PATH, "w", encoding="utf-8") as f:
            json.dump(state, f, ensure_ascii=False, indent=2)
    except Exception:
        pass

STATE = _load_state()
atexit.register(lambda: _save_state(STATE))

def _now_str():
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")

def _is_quiet_hours():
    if not QUIET_HOURS["enabled"]:
        return False
    h = datetime.now().hour
    s, e = QUIET_HOURS["start_hour"], QUIET_HOURS["end_hour"]
    if s < e:
        return s <= h < e
    else:
        return h >= s or h < e

def _dedup_ok(label, minutes):
    last = STATE["last_notified"].get(label)
    if last:
        last_dt = datetime.fromisoformat(last)
        if datetime.now() - last_dt < timedelta(minutes=minutes):
            return False
    return True

def _touch_notified(label):
    STATE["last_notified"][label] = datetime.now().isoformat()
    _save_state(STATE)

# ========== 📍 위치/날씨/시간 유틸 ==========
def get_current_location():
    # 실제 앱에서는 GPS API 사용 / 현재는 테스트용 mock 좌표
    return (37.3310, 127.1425)

def meters_between(a, b):
    try:
        return geodesic(a, b).meters
    except Exception:
        return float("inf")

def check_weather(city):
    try:
        url = "http://api.weatherapi.com/v1/current.json"
        params = {"key": WEATHER_API_KEY, "q": city, "lang": "ko", "aqi": "no"}
        r = requests.get(url, params=params, timeout=HTTP_TIMEOUT_SEC)
        r.raise_for_status()
        data = r.json()
        condition = data["current"]["condition"]["text"]
        precip_mm = data["current"].get("precip_mm", 0)
        rainy_words = ["비", "소나기", "뇌우", "천둥", "폭우", "우박"]
        is_rain = any(w in condition for w in rainy_words) or (precip_mm and precip_mm > 0)
        print(f"[{_now_str()}] 날씨: {city} → '{condition}', 강수량 {precip_mm}mm, 비판단={is_rain}")
        return is_rain, condition
    except Exception as e:
        print(f"[{_now_str()}] 날씨 API 오류: {e}")
        return False, None

def send_notification(title, message):
    if _is_quiet_hours():
        print(f"[{_now_str()}] (조용시간) 알림 보류: [{title}] {message}")
        return
    print(f"\n🔔 [{title}] {message}\n")

# ========== ⏰ 체크 로직 ==========
def time_check():
    now_hm = datetime.now().strftime("%H:%M")
    for item in reminders:
        if item["type"] == "time" and item["time"] == now_hm:
            label = f"시간-{item['label']}"
            if _dedup_ok(label, DEDUP_MINUTES_DEFAULT):
                send_notification("시간 알림", f"{item['label']} 시간이에요! ({now_hm})")
                _touch_notified(label)

def weather_check():
    for item in reminders:
        if item["type"] == "weather":
            ok, condition = check_weather(item["location"])
            label = f"날씨-{item['label']}-{item['location']}"
            if ok and _dedup_ok(label, DEDUP_MINUTES_WEATHER):
                send_notification("날씨 알림", f"{item['label']} 챙기세요! ({item['location']} : {condition})")
                _touch_notified(label)

def location_check():
    current = get_current_location()
    for item in reminders:
        if item["type"] != "location":
            continue
        label = f"위치-{item['label']}"
        target = tuple(item["target"])
        dist = meters_between(current, target)
        inside = dist <= GEOFENCE_METERS
        prev_inside = STATE["geo_inside"].get(label, True)

        print(f"[{_now_str()}] 위치체크 '{item['label']}': 현재거리 {dist:.1f}m "
              f"({'반경내' if inside else '반경외'}), 기준={GEOFENCE_METERS}m")

        if prev_inside and not inside:
            if _dedup_ok(label, DEDUP_MINUTES_DEFAULT):
                send_notification("위치 알림", f"{item['label']} 안 챙긴 것 같아요! (기준 위치에서 {dist:.0f}m)")
                _touch_notified(label)
        STATE["geo_inside"][label] = inside
    _save_state(STATE)

# ========== 🔁 스케줄러 ==========
def run_scheduler_forever():
    schedule.every(1).minutes.do(time_check)
    schedule.every(1).minutes.do(location_check)
    schedule.every(WEATHER_CHECK_MINUTES).minutes.do(weather_check)

    time_check()
    weather_check()
    location_check()

    print("✅ 리마인더 기능 실행 시작 (Ctrl+C로 종료)\n")
    try:
        while True:
            schedule.run_pending()
            time.sleep(1)
    except KeyboardInterrupt:
        print("\n👋 종료합니다.")
    finally:
        _save_state(STATE)

# ========== 🧠 자동 저장 ==========
code_text = open(__file__ if "__file__" in globals() else "reminder_app.py", "r", encoding="utf-8").read()
with open("reminder_app.py", "w", encoding="utf-8") as f:
    f.write(code_text)
print("💾 reminder_app.py 저장 완료")

# ========== 🚀 실행 ==========
if __name__ == "__main__":
    run_scheduler_forever()
