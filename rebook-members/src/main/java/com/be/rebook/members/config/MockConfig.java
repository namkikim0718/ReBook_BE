package com.be.rebook.members.config;

import com.be.rebook.auth.jwt.JWTUtil;
import com.be.rebook.auth.repository.RefreshTokensRepository;
import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MockConfig {

    @Bean
    public JWTUtil mockjwtUtil(){
        return Mockito.mock(JWTUtil.class);
    }

    @Bean
    public RefreshTokensRepository mockrefreshTokensRepository(){
        return Mockito.mock(RefreshTokensRepository.class);
    }
}
