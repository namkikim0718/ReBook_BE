package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.dto.CustomUserDetails;
import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.repository.MembersRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class CustomUserDetailService implements UserDetailsService {

    private final MembersRepository membersRepository;

    private static final Logger customUserDetailServiceLogger = LoggerFactory.getLogger(CustomUserDetailService.class);

    public CustomUserDetailService(MembersRepository membersRepository){
        this.membersRepository = membersRepository;
    }
    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        Members userData = membersRepository.findByUsername(username);
        customUserDetailServiceLogger.info("디비에서 인증받아왔고 이제 받은 데이터 가공해서 필터로 보낼준비중임");
        if(userData != null){
            return new CustomUserDetails(userData);
        }

        return null;
    }
}
