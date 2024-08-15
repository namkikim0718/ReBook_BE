package com.be.rebook.auth.jwt.type;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import jakarta.annotation.PostConstruct;

@Configuration
public class TokenExpiryConfig {

    @Value("${jwt.access-token-expiration}")
    private Long accessExpiry;

    @Value("${jwt.refresh-token-expiration}")
    private Long refreshExpiry;

    @Value("${jwt.mailauth-token-expiration")
    private Long mailauthExpiry;


    @PostConstruct
    public void init() {
        TokenCategory.ACCESS.setExpiry(accessExpiry);
        TokenCategory.REFRESH.setExpiry(refreshExpiry);
        TokenCategory.MAILAUTH.setExpiry(mailauthExpiry);
    }
}
