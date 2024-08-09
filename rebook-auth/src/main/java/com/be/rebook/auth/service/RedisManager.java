package com.be.rebook.auth.service;

import org.springframework.stereotype.Service;

// Redis에 직접 접근하는 기능을 관리하는 인터페이스
@Service
public interface RedisManager {
    void setValues(String key, String value);

    //key, value, 시간, 시간단위
    //e.g. test@naver.com 123456 3 TimeUnit.MINUTES
    void setValuesWithDuration(String key, String value);

    String getValue(String key);

    void deleteValue(String key);
}
