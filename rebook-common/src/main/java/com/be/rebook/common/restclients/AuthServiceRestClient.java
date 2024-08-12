package com.be.rebook.common.restclients;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.service.annotation.PostExchange;

import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;

public interface AuthServiceRestClient {

    @PostExchange("/auth/members/authenticate")
    ResponseEntity<BaseResponse<MemberLoginInfo>> authenticate(@RequestHeader("Authorization") String token);
}
